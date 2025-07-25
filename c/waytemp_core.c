#define _POSIX_C_SOURCE 200809L
#include "waytemp_core.h"
#include "wlr-gamma-control-unstable-v1-client-protocol.h"
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>

#define M_PI 3.14159265358979323846

struct output {
  struct wl_list link;
  struct wl_output *wl_output;
  struct zwlr_gamma_control_v1 *gamma_control;
  uint32_t ramp_size;
  int table_fd;
  uint16_t *table;
  time_t last_retry;
  int retry_count;
};

struct waytemp_ctx {
  struct wl_display *display;
  struct wl_registry *registry;
  struct zwlr_gamma_control_manager_v1 *gamma_control_manager;
  struct wl_list outputs;
  int last_temp;
  double last_gamma;
};

static int illuminant_d(int temp, double *x, double *y) {
  if (temp >= 2500 && temp <= 7000) {
    *x = 0.244063 + 0.09911e3 / temp + 2.9678e6 / pow(temp, 2) -
         4.6070e9 / pow(temp, 3);
  } else if (temp > 7000 && temp <= 25000) {
    *x = 0.237040 + 0.24748e3 / temp + 1.9018e6 / pow(temp, 2) -
         2.0064e9 / pow(temp, 3);
  } else {
    return -1;
  }
  *y = (-3 * pow(*x, 2)) + (2.870 * (*x)) - 0.275;
  return 0;
}

static int planckian_locus(int temp, double *x, double *y) {
  if (temp >= 1667 && temp <= 4000) {
    *x = -0.2661239e9 / pow(temp, 3) - 0.2343589e6 / pow(temp, 2) +
         0.8776956e3 / temp + 0.179910;
    if (temp <= 2222) {
      *y = -1.1064814 * pow(*x, 3) - 1.34811020 * pow(*x, 2) +
           2.18555832 * (*x) - 0.20219683;
    } else {
      *y = -0.9549476 * pow(*x, 3) - 1.37418593 * pow(*x, 2) +
           2.09137015 * (*x) - 0.16748867;
    }
  } else if (temp > 4000 && temp < 25000) {
    *x = -3.0258469e9 / pow(temp, 3) + 2.1070379e6 / pow(temp, 2) +
         0.2226347e3 / temp + 0.240390;
    *y = 3.0817580 * pow(*x, 3) - 5.87338670 * pow(*x, 2) + 3.75112997 * (*x) -
         0.37001483;
  } else {
    return -1;
  }
  return 0;
}

static double srgb_gamma(double value, double gamma) {
  if (value <= 0.0031308) {
    return 12.92 * value;
  } else {
    return pow(1.055 * value, 1.0 / gamma) - 0.055;
  }
}

static double clamp(double value) {
  if (value > 1.0) {
    return 1.0;
  } else if (value < 0.0) {
    return 0.0;
  } else {
    return value;
  }
}

static void calc_whitepoint(int temp, double *r, double *g, double *b) {
  if (temp == 6500) {
    *r = *g = *b = 1.0;
    return;
  }

  double x, y, z;

  if (temp >= 25000) {
    illuminant_d(25000, &x, &y);
  } else if (temp >= 4000) {
    illuminant_d(temp, &x, &y);
  } else if (temp >= 2500) {
    double x1, y1, x2, y2;
    illuminant_d(temp, &x1, &y1);
    planckian_locus(temp, &x2, &y2);

    double factor = (4000.0 - temp) / 1500.0;
    double sinefactor = (cos(M_PI * factor) + 1.0) / 2.0;
    x = x1 * sinefactor + x2 * (1.0 - sinefactor);
    y = y1 * sinefactor + y2 * (1.0 - sinefactor);
  } else {
    planckian_locus(temp >= 1667 ? temp : 1667, &x, &y);
  }
  z = 1.0 - x - y;

  // Convert XYZ to RGB
  *r = srgb_gamma(clamp(3.2404542 * x - 1.5371385 * y - 0.4985314 * z), 2.2);
  *g = srgb_gamma(clamp(-0.9692660 * x + 1.8760108 * y + 0.0415560 * z), 2.2);
  *b = srgb_gamma(clamp(0.0556434 * x - 0.2040259 * y + 1.0572252 * z), 2.2);

  // Normalize
  double maxw = fmax(*r, fmax(*g, *b));
  *r /= maxw;
  *g /= maxw;
  *b /= maxw;
}

static int create_gamma_table(uint32_t ramp_size, uint16_t **table) {
  size_t size = ramp_size * 3 * sizeof(uint16_t);
  char template[] = "/tmp/waytemp-XXXXXX";
  int fd = mkstemp(template);
  if (fd < 0)
    return -1;

  ftruncate(fd, size);
  unlink(template);

  void *data = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (data == MAP_FAILED) {
    close(fd);
    return -1;
  }

  *table = data;
  return fd;
}

static void gamma_control_handle_gamma_size(
    void *data, struct zwlr_gamma_control_v1 *control, uint32_t size) {
  struct output *output = data;
  output->ramp_size = size;
  if (size > 0) {
    if (output->table_fd >= 0) {
      close(output->table_fd);
    }
    output->table_fd = create_gamma_table(size, &output->table);
    output->retry_count = 0; // Reset retry count on success
  }
}

static void gamma_control_handle_failed(void *data,
                                        struct zwlr_gamma_control_v1 *control) {
  struct output *output = data;
  fprintf(stderr, "gamma control failed, will retry\n");
  zwlr_gamma_control_v1_destroy(output->gamma_control);
  output->gamma_control = NULL;
  output->last_retry = time(NULL);
  output->retry_count++;
}

static const struct zwlr_gamma_control_v1_listener gamma_control_listener = {
    .gamma_size = gamma_control_handle_gamma_size,
    .failed = gamma_control_handle_failed,
};

static void registry_handle_global(void *data, struct wl_registry *registry,
                                   uint32_t name, const char *interface,
                                   uint32_t version) {
  struct waytemp_ctx *ctx = data;

  if (strcmp(interface, wl_output_interface.name) == 0) {
    struct output *output = calloc(1, sizeof(struct output));
    output->wl_output =
        wl_registry_bind(registry, name, &wl_output_interface, 1);
    output->table_fd = -1;
    output->last_retry = 0;
    output->retry_count = 0;
    wl_list_insert(&ctx->outputs, &output->link);
  } else if (strcmp(interface, zwlr_gamma_control_manager_v1_interface.name) ==
             0) {
    ctx->gamma_control_manager = wl_registry_bind(
        registry, name, &zwlr_gamma_control_manager_v1_interface, 1);
  }
}

static void registry_handle_global_remove(void *data,
                                          struct wl_registry *registry,
                                          uint32_t name) {
  // Ignore for now
}

static const struct wl_registry_listener registry_listener = {
    .global = registry_handle_global,
    .global_remove = registry_handle_global_remove,
};

static void setup_gamma_control(struct waytemp_ctx *ctx,
                                struct output *output) {
  if (!ctx->gamma_control_manager || output->gamma_control) {
    return;
  }

  output->gamma_control = zwlr_gamma_control_manager_v1_get_gamma_control(
      ctx->gamma_control_manager, output->wl_output);
  zwlr_gamma_control_v1_add_listener(output->gamma_control,
                                     &gamma_control_listener, output);
}

waytemp_ctx *waytemp_init(void) {
  struct waytemp_ctx *ctx = calloc(1, sizeof(struct waytemp_ctx));
  wl_list_init(&ctx->outputs);
  ctx->last_temp = 6500;
  ctx->last_gamma = 1.0;

  ctx->display = wl_display_connect(NULL);
  if (!ctx->display) {
    free(ctx);
    return NULL;
  }

  ctx->registry = wl_display_get_registry(ctx->display);
  wl_registry_add_listener(ctx->registry, &registry_listener, ctx);
  wl_display_roundtrip(ctx->display);

  if (!ctx->gamma_control_manager) {
    waytemp_destroy(ctx);
    return NULL;
  }

  // Setup gamma controls for all outputs
  struct output *output;
  wl_list_for_each(output, &ctx->outputs, link) {
    setup_gamma_control(ctx, output);
  }
  wl_display_roundtrip(ctx->display);

  return ctx;
}

void waytemp_destroy(waytemp_ctx *ctx) {
  if (!ctx)
    return;

  struct output *output, *tmp;
  wl_list_for_each_safe(output, tmp, &ctx->outputs, link) {
    if (output->gamma_control)
      zwlr_gamma_control_v1_destroy(output->gamma_control);
    if (output->table_fd >= 0)
      close(output->table_fd);
    wl_list_remove(&output->link);
    free(output);
  }

  if (ctx->display)
    wl_display_disconnect(ctx->display);
  free(ctx);
}

int waytemp_set_temperature(waytemp_ctx *ctx, int temp, double gamma) {
  double r, g, b;
  calc_whitepoint(temp, &r, &g, &b);

  fprintf(stderr, "Setting temp %dK: R=%.3f G=%.3f B=%.3f\n", temp, r, g, b);

  // Store last values for retry
  ctx->last_temp = temp;
  ctx->last_gamma = gamma;

  struct output *output;
  wl_list_for_each(output, &ctx->outputs, link) {
    // Retry failed outputs after 2 seconds
    if (!output->gamma_control && output->retry_count < 10) {
      time_t now = time(NULL);
      if (now - output->last_retry >= 2) {
        fprintf(stderr, "Retrying gamma control setup (attempt %d)\n",
                output->retry_count + 1);
        setup_gamma_control(ctx, output);
        wl_display_roundtrip(ctx->display);
      }
    }

    if (!output->gamma_control || output->table_fd < 0 ||
        output->ramp_size == 0)
      continue;

    uint16_t *red = output->table;
    uint16_t *green = output->table + output->ramp_size;
    uint16_t *blue = output->table + 2 * output->ramp_size;

    for (uint32_t i = 0; i < output->ramp_size; i++) {
      double val = (double)i / (output->ramp_size - 1);
      red[i] = (uint16_t)(UINT16_MAX * pow(val * r, 1.0 / gamma));
      green[i] = (uint16_t)(UINT16_MAX * pow(val * g, 1.0 / gamma));
      blue[i] = (uint16_t)(UINT16_MAX * pow(val * b, 1.0 / gamma));
    }

    lseek(output->table_fd, 0, SEEK_SET);
    zwlr_gamma_control_v1_set_gamma(output->gamma_control, output->table_fd);
  }

  wl_display_flush(ctx->display);
  return 0;
}

int waytemp_process_events(waytemp_ctx *ctx) {
  int ret = wl_display_dispatch(ctx->display);

  // Periodically retry setting temperature in case of failures
  static time_t last_retry_check = 0;
  time_t now = time(NULL);
  if (now - last_retry_check >= 5) {
    last_retry_check = now;

    // Check if any outputs need retry
    struct output *output;
    int need_retry = 0;
    wl_list_for_each(output, &ctx->outputs, link) {
      if (!output->gamma_control && output->retry_count < 10) {
        need_retry = 1;
        break;
      }
    }

    if (need_retry) {
      waytemp_set_temperature(ctx, ctx->last_temp, ctx->last_gamma);
    }
  }

  return ret;
}
