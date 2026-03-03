all: update-all

sync:
	@. "./sync.sh"

update: sync
	@. "./update.sh"

install: sync
	@. "./install.sh"

actions:
	@sed '/^[A-Za-z/\-]*:/!d;s/:.*$$//g' Makefile
