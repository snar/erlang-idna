REBAR?=./rebar

all: build

clean:
	@$(REBAR) clean

distclean: clean
	@rm -rf deps

build:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

test:
	@./test.escript

dataupdate:
	@./bootstrap.sh


.PHONY: test deps
