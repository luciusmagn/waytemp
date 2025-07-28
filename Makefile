DESTDIR ?=
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib

LISP ?= sbcl
LISP_BUILD_CMD = $(LISP) --non-interactive \
                         --eval "(declaim (optimize (speed 3) (safety 3) (debug 3)))" \
                         --eval "(push *default-pathname-defaults* asdf:*central-registry*)" \
                         --eval "(ql:quickload :waytemp)" \
                         --eval "(asdf:make :waytemp)" \
                         --quit

.PHONY: all clean install uninstall

all: waytemp

waytemp: c/libwaytemp_core.so
	$(LISP_BUILD_CMD)

c/libwaytemp_core.so:
	$(MAKE) -C c

clean:
	$(MAKE) -C c clean
	rm -f waytemp

install: waytemp c/libwaytemp_core.so
	install -D -m 755 waytemp $(DESTDIR)$(BINDIR)/waytemp
	install -D -m 644 c/libwaytemp_core.so $(DESTDIR)$(LIBDIR)/libwaytemp_core.so

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/waytemp
	rm -f $(DESTDIR)$(LIBDIR)/libwaytemp_core.so

dev: c/libwaytemp_core.so
	$(LISP) --eval "(ql:quickload :waytemp)"
