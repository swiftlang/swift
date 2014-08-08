##===--- SwiftIntTypes.py -----------------------------*- coding: utf-8 -*-===##
##
## This source file is part of the Swift.org open source project
##
## Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
## Licensed under Apache License v2.0 with Runtime Library Exception
##
## See http://swift.org/LICENSE.txt for license information
## See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
##
##===----------------------------------------------------------------------===##

# Bit counts for all int types
_all_integer_type_bitwidths = [8, 16, 32, 64, 'Int']

# Number of bits in the biggest int type
int_max_bits = 64

def all_integer_types(word_bits):
    for name in _all_integer_type_bitwidths:
        for signed in False, True:
            yield str(name), int(word_bits if name=='Int' else name), signed

def all_integer_type_names():
    for name in _all_integer_type_bitwidths:
        for signed in False, True:
            yield int_name(str(name), signed)

def _base_int_name(name):
    return 'Int' if name == 'Int' else 'Int' + str(name)

def builtin_int_name(name):
    return 'Word' if name == 'Int' else 'Int' + str(name)

def int_name(name, signed):
    return ('' if signed else 'U') + _base_int_name(name)

def other_int_name(name, signed):
    return ('U' if signed else '') + _base_int_name(name)

def is_signed_int(name):
    return name[0] != 'U'

