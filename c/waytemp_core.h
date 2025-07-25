#ifndef WAYTEMP_CORE_H
#define WAYTEMP_CORE_H

typedef struct waytemp_ctx waytemp_ctx;

waytemp_ctx *waytemp_init(void);
void waytemp_destroy(waytemp_ctx *ctx);
int waytemp_set_temperature(waytemp_ctx *ctx, int temp, double gamma);
int waytemp_process_events(waytemp_ctx *ctx);

#endif
