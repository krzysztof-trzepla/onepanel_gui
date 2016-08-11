all: deps compile

.PHONY: deps
deps:
	./rebar get-deps

.PHONY: compile
compile:
	./rebar compile

.PHONY: clean
clean:
	./rebar clean

##
## Dialyzer targets local
##

PLT ?= .dialyzer.plt

.PHONY: plt
plt:
	dialyzer --check_plt --plt ${PLT}; \
	if [ $$? != 0 ]; then \
		dialyzer --build_plt --output_plt ${PLT} --apps kernel stdlib sasl erts \
		ssl tools runtime_tools crypto inets xmerl snmp public_key eunit \
		common_test test_server syntax_tools compiler edoc mnesia hipe \
		ssh webtool -r deps; \
	fi; exit 0

.PHONY: dialyzer
dialyzer: plt
	dialyzer ./ebin --plt ${PLT} -Werror_handling --fullpath