#!/bin/sh
echo " "
echo "#### pyflakes ####"
pyflakes $1
echo "##### pep8 ####"
pep8 $1
