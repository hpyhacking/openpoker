start_dev:
	rebar compile
	erl -sname genesis -pa ebin -pa deps/*/ebin -s genesis_app
