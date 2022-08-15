make:
	make client&
	make server

client:
	echo '#lang racket\n(require "src/run-nmbs.rkt")(run)' > client.tmp.rkt
	raco exe -o client client.tmp.rkt
	rm -f client.tmp.rkt

server:
	echo '#lang racket\n(require "src/run-server.rkt")(run)' > server.tmp.rkt
	raco exe -o server server.tmp.rkt
	rm -f server.tmp.rkt

.PHONY: test client server
test:
	cd src; racket test-suite.rkt

