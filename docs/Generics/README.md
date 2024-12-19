# Compiling Swift Generics

This is a book about the *implementation* of generic programming--also known as parametric polymorphism--in the Swift compiler. The first four chapters also give an overview of the Swift compiler architecture in general.

## Downloading the PDF

A periodically-updated PDF is available here:

> https://download.swift.org/docs/assets/generics.pdf

## Typesetting the PDF

It's written in TeX, so to typeset the PDF yourself, you need a TeX distribution:

- [MacTeX](https://www.tug.org/mactex/mactex-download.html): macOS
- [TeX Live](https://www.tug.org/texlive/): Linux, Windows
- [MikTeX](https://miktex.org): another alternative for macOS, Linux, Windows

### Using `make`

Running `make` in `docs/Generics/` will run `pdflatex` and `bibtex` in the right order to generate the final document with bibliography, index and cross-references:

```
cd docs/Generics/
make
```

### Using `latexmk`

A more modern alternative is to use `latexmk`, which runs `pdflatex` and `bibtex` until fixed point:

```
cd docs/Generics/
latexmk -pdf generics.tex
```

### Manually

You can also just do this:

```
cd docs/Generics/
pdflatex generics
bibtex generics
pdflatex generics
pdflatex generics
```

## Reading the PDF

The book makes use of internal hyperlinks so it is is best to use PDF reader with support for PDF bookmarks and back/forward history:

- Preview.app on macOS fits the bill; you can add Back/Forward buttons to the toolbar with **View** > **Customize Toolbar**.
- [Skim.app](https://skim-app.sourceforge.io) is a BSD-licensed open source PDF reader for macOS.

The font size and link targets are probably too small for a smartphone display, so I recommend using something bigger.

## Current Status

This is a work in progress.

The following chapters need some editing:

- Part II:
  - Substitution Maps
- Part IV:
  - Completion

The following chapters are not yet written:

- Part III:
  - Opaque Return Types
  - Existential Types
- Part IV:
  - The Property Map
  - Rule Minimization
