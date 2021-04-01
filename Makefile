PWSH = pwsh

.PHONY: build
build: build-deps
	cabal v2-build

.PHONY: build-deps
build-deps:
	cabal v2-build --only-dependencies

.PHONY: test
test: doctest spec

.PHONY: doctest
doctest: build-deps
	cabal v2-test doctest

.PHONY: spec
spec: build-deps
	cabal v2-test spec

.PHONY: repl
repl:
	cabal v2-repl

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse src, app, test | ForEach-Object { stylish-haskell -i $$_.FullName } }"

.PHONY: lint
lint:
	hlint src app

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: clean
clean:
	cabal v2-clean

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content .\Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
