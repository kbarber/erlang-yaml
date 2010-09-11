# Copyright 2010 Bob.sh

all: 
	@mkdir -p ebin/
	@erl -make

	@erlc 	-Iebin/ \
		-o ebin \
		src/*.erl

clean: 
	rm -f ebin/*.beam
	rm -f ebin/*.boot
	rm -f ebin/*.script
	rm -fr doc/api
	rm -f erl_crash*

test: all
	@erl	-pa ebin \
		-smp auto \
		-noshell \
		-noinput \
		-s yaml_tests \
		start
