# Tomas Petricek's CV generator

I can no longer cope with maintaining of up-to-date academic CVs in the billions of possible required formats. Procrastinating with this instead!
It turns the source data in `data` (written in machine readable Markdown) into various formats of CVs in `source` and generates nicely formatted CVs in `docs`.

* [`data`](https://github.com/tpetricek/cv/tree/master/data) contains various Markdown files with various academic things that one may have on their CV. The `build.fsx` script reads this and parses it a nice F# [domain model](https://github.com/tpetricek/cv/blob/master/build.fsx#L13).
* [`sources`](https://github.com/tpetricek/cv/tree/master/sources) contains Liquid template, processed using the [fluid package](https://github.com/sebastienros/fluid).
* CV templates can easily include various components from the [`templates`](https://github.com/tpetricek/cv/tree/master/templates) so you never ever have to copy paste!
* There is some [fine-tuned CSS](https://github.com/tpetricek/cv/blob/master/templates/styles/default.liquid) that displays the CV nicely and hopefully makes it print nicely(ish) too.

## Various CVs

* [Academic version](https://tpetricek.github.io/cv/academic.html) - ~2 page selection of the
  most important academic things
* [Complete version](https://tpetricek.github.io/cv/complete.html) - ~5 page version with
  everything but not publications
* [Admin version](https://tpetricek.github.io/cv/brief.html) - ~3 page version focused on
  teaching and academic service
* [Interdisciplinary version](https://tpetricek.github.io/cv/interdisciplinary.html) - ~2 page
  version with interdisciplinary publications
* [Publications list](https://tpetricek.github.io/cv/publications.html) - all the publications
* [Habilitation materials](https://tpetricek.github.io/cv/habilitation/) - lots of tedious paperwork for promotion
