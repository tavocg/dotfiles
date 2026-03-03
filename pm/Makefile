all: update

sync:
	@. "./sync.sh"

update: sync
	@. "./update.sh"

install: sync
	@. "./install.sh"

actions:
	@sed '/^[A-Za-z/\-]*:/!d;s/:.*$$//g' Makefile
