# Tomas Petricek's CV generator

I can no longer cope with maintaining of up-to-date academic CVs in the billions of possible required formats. Procrastinating with this instead! 
It turns the source data in `data` (written in machine readable Markdown) into various formats of CVs in `source` and generates nicely formatted CVs in `docs`. 

* [`data`](https://github.com/tpetricek/cv/tree/master/data) contains various Markdown files with various academic things that one may have on their CV. The `build.fsx` script reads this and parses it a nice F# [domain model](https://github.com/tpetricek/cv/blob/master/build.fsx#L13).
* [`sources`](https://github.com/tpetricek/cv/tree/master/sources) contains Liquid template, processed using the [fluid package](https://github.com/sebastienros/fluid).
* CV templates can easily include various components from the [`templates`](https://github.com/tpetricek/cv/tree/master/templates) so you never ever have to copy paste things!
* There is some [fine-tuned CSS](https://github.com/tpetricek/cv/blob/master/templates/styles/default.liquid) that displays the CV nicely and hopefully makes it print nicely(ish) too.
 
## Various CVs

* [Complete version](https://tpetricek.github.io/cv/complete.html) with everything in it
* [Publications list](https://tpetricek.github.io/cv/publications.html) with everything in it

## Habilitation materials

* [Professional highlights](https://tpetricek.github.io/cv/habilitation/highlights.html) ([PDF](https://tpetricek.github.io/cv/habilitation/highlights.pdf))
* [Curriculum vitae](https://tpetricek.github.io/cv/habilitation/cv.html) ([PDF](https://tpetricek.github.io/cv/habilitation/cv.pdf))
* [List of publications](https://tpetricek.github.io/cv/habilitation/publications.html) ([PDF](https://tpetricek.github.io/cv/habilitation/habilitation.pdf))
* [Citation report](https://tpetricek.github.io/cv/habilitation/citations.html) ([PDF](https://tpetricek.github.io/cv/habilitation/citations.pdf))
