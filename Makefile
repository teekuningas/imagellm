.PHONY: build
build:
	rm -fr dist/*
	node_modules/.bin/parcel build index.html

.PHONY: watch
watch: node_modules
	rm -fr .parcel-cache && PARCEL_ELM_NO_DEBUG=1 npm start

.PHONY: prettier
prettier:
	./node_modules/.bin/elm-format src/*

.PHONY: node_modules
node_modules:
	npm ci
	touch node_modules

.PHONY: shell
shell:
	nix develop

.PHONY: nix-%
nix-%:
	nix develop \
		--command $(MAKE) $*

FORCE:
