make:
	make client&
	make server

client:
	echo '#lang racket\n(require "src/nmbs-client.rkt")(run)' > client.tmp.rkt
	raco exe -o nmbs-client client.tmp.rkt
	rm -f client.tmp.rkt

server:
	echo '#lang racket\n(require "src/infrabel-server.rkt")(run)' > server.tmp.rkt
	raco exe -o infrabel-server server.tmp.rkt
	rm -f server.tmp.rkt

.PHONY: test
test:
	cd src; racket test-suite.rkt

