
submodules:
	raco make submodules.scrbl
	scribble --pdf submodules.scrbl

html:
	raco make submodules.scrbl
	scribble --dest /tmp --html submodules.scrbl

full-model:
	raco make full-model.scrbl
	scribble --pdf full-model.scrbl

zip:
	rm -f model.zip
	zip model.zip model.rkt syntax-rule.rkt

TMPSRC=/tmp/submod-src

src:
	rm -rf $(TMPSRC)
	cp -r . $(TMPSRC)
	rm -rf $(TMPSRC)/compiled
	rm -rf $(TMPSRC)/.svn
	rm -rf $(TMPSRC)/*~
	rm -rf $(TMPSRC)/*.pdf
	rm -rf $(TMPSRC)/*.zip
	cd /tmp; zip -r submod-src.zip submod-src
	cp /tmp/submod-src.zip .
