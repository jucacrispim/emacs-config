#!/bin/sh


install_python_modules(){
    echo "Installing rope, ropemacs and ropemode"
    pip install $ROPE $ROPEMODE $ROPEMACS
}

PYVERSION=`python --version 2>&1 | cut -d ' ' -f2 | cut -d'.' -f1`;

if [ $(($PYVERSION < 3)) = '0' ]  # pyton3
then
    ROPE='rope_py3k';
    ROPEMACS='ropemacs3k';
    ROPEMODE='ropemode3k';
else  #python 2
    ROPE='rope';
    ROPEMACS='ropemacs';
    ROPEMODE='ropemode';
fi;

install_python_modules
