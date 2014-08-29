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
_all_integer_type_bitwidths_new = [8, 16, 32, 64]

# Number of bits in the biggest int type
int_max_bits = max(_all_integer_type_bitwidths_new)

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

class SwiftIntegerType(object):
    def __init__(self, is_word, bits, is_signed):
        self.is_word = is_word
        self.bits = bits
        self.is_signed = is_signed

        if is_word:
            self.possible_bitwidths = [ 32, 64 ]
        else:
            self.possible_bitwidths = [ bits ]

        # Derived properties
        self.stdlib_name = \
            ('' if is_signed else 'U') + \
            'Int' + \
            ('' if is_word else str(bits))

        self.builtin_name = ('Word' if is_word else 'Int' + str(bits))

    def get_opposite_signedness(self):
        return SwiftIntegerType(self.is_word, self.bits, not self.is_signed)

    def __eq__(self, other):
        return self.is_word == other.is_word and \
            self.bits == other.bits and \
            self.is_signed == other.is_signed

    def __ne__(self, other):
        return not self.__eq__(other)

def all_integer_types_new(word_bits):
    for bitwidth in _all_integer_type_bitwidths_new:
        for is_signed in [ False, True ]:
            yield SwiftIntegerType(is_word=False, bits=bitwidth,
                is_signed=is_signed)

    for is_signed in [ False, True ]:
        yield SwiftIntegerType(is_word=True, bits=word_bits,
            is_signed=is_signed)

# 'truncatingBitPattern' initializer is defined if the conversion is truncating
# on any platform that Swift supports.
def should_define_truncating_bit_pattern_init(src_ty, dst_ty):
    # Don't define a truncating conversion between a type and itself.
    if src_ty == dst_ty:
        return False

    # Conversion to opposite signedness is never truncating.
    if src_ty == dst_ty.get_opposite_signedness():
        return False

    for src_ty_bits in src_ty.possible_bitwidths:
        for dst_ty_bits in dst_ty.possible_bitwidths:
            if src_ty_bits > dst_ty_bits:
                return True

    return False

