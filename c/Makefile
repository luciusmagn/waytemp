CC = gcc
CFLAGS = -fPIC -O2 -Wall -I.
LIBS = -lwayland-client -lm

PROTOCOL_H = wlr-gamma-control-unstable-v1-client-protocol.h
PROTOCOL_C = wlr-gamma-control-unstable-v1-client-protocol.c

all: libwaytemp_core.so

$(PROTOCOL_H): wlr-gamma-control-unstable-v1.xml
	wayland-scanner client-header $< $@

$(PROTOCOL_C): wlr-gamma-control-unstable-v1.xml
	wayland-scanner private-code $< $@

libwaytemp_core.so: waytemp_core.c $(PROTOCOL_C) $(PROTOCOL_H)
	$(CC) $(CFLAGS) -shared -o $@ waytemp_core.c $(PROTOCOL_C) $(LIBS)

clean:
	rm -f libwaytemp_core.so $(PROTOCOL_H) $(PROTOCOL_C)

.PHONY: all clean
