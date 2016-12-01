#!/bin/bash

pdflatex presento.tex
bibtex presento.aux
pdflatex presento.tex
pdflatex presento.tex
