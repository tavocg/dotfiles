all: update-all

sync-all:
	@. "./sync-all.sh"

update-all: sync-all
	@. "./update-all.sh"

install-all: sync-all
	@. "./install-all.sh"

actions:
	@sed '/^[A-Za-z/\-]*:/!d;s/:.*$$//g' Makefile
