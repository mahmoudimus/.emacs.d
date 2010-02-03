#!/usr/bin/env python
# -*- coding: utf-8 -*-

from distutils.core import setup

setup(name='Rebox', version='Pymacs-0.23',
      description="Refilling comment boxes from within Emacs.",
      author='François Pinard', author_email='pinard@iro.umontreal.ca',
      url='http://pinard.progiciels-bpi.ca',
      scripts=['rebox'],
      py_modules=['Pymacs.rebox'])
