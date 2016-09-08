all: compile

.PHONY: compile
compile:
	./rebar3 compile

.PHONY: clean
clean:
	./rebar3 clean

##
## Dialyzer targets local
##

# Dialyzes the project.
dialyzer:
	$(REBAR) dialyzer