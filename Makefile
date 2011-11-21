VERSION=1.1

release:
	mkdir -p tron-${VERSION}/src
	cp README tron-${VERSION}
	cp src/tangleit src/weaveit src/tronapi.web src/logo_tron_black.eps tron-${VERSION}/src/
	tar cvJf tron-${VERSION}.tar.xz tron-${VERSION}
	mv tron-${VERSION}.tar.xz releases/
	rm -r tron-${VERSION}

apirelease:
	mkdir tronapi-${VERSION}
	cp src/tronapi.ss src/tronapi.pdf tronapi-${VERSION}
	tar cvJf tronapi-${VERSION}.tar.xz tronapi-${VERSION}
	mv tronapi-${VERSION}.tar.xz releases
	rm -r tronapi-${VERSION}
