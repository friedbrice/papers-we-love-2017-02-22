all: summary.html presentation.pdf Sampler

summary.html: summary.md
	pandoc -t html5 --standalone --mathjax -o summary.html summary.md

presentation.pdf: presentation.md
	pandoc -t beamer -o presentation.pdf presentation.md

Sampler: demo/Beta.hs demo/Models.hs demo/Sampler.hs
	cd demo && ghc Sampler.hs && cd .. && mv demo/Sampler .

clean:
	git clean -xf
