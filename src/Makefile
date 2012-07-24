.SUFFIXES: .czw .tex .ss .pdf

tronapi: tronapi.ss tronapi.pdf

clean:
	rm -rf tronapi.{ss,tex,pdf,log,scn,idx,toc}
	rm -rf missfont.log

.czw.ss:
	cheztangle $<

.czw.tex:
	chezweave $<

.tex.pdf:
	pdftex $*
