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
_all_integer_type_bitwidths = [8, 16, 32, 64]

# Number of bits in the biggest int type
int_max_bits = max(_all_integer_type_bitwidths)

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

        self.builtin_name = 'Int' + str(bits)

    def get_opposite_signedness(self):
        return SwiftIntegerType(self.is_word, self.bits, not self.is_signed)

    def __eq__(self, other):
        return self.is_word == other.is_word and \
            self.bits == other.bits and \
            self.is_signed == other.is_signed

    def __ne__(self, other):
        return not self.__eq__(other)

def all_integer_types(word_bits):
    for bitwidth in _all_integer_type_bitwidths:
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

def all_integer_type_names():
    return [self_ty.stdlib_name for self_ty in all_integer_types(0)]

def all_real_number_type_names():
    return ['Float', 'Double'] #FIXME , 'Float80' Revert until I figure out a test failure  # Float80 for i386 & x86_64

def all_numeric_type_names():
    return all_integer_type_names() + all_real_number_type_names()

def numeric_type_names_Macintosh_only():
    return ['Float80']  

# Swift_Programming_Language/Expressions.html

def all_integer_binary_operator_names():
    return ['<<', '>>', '&*', '&', '&+', '&-', '|', '^']
    
def all_integer_or_real_binary_operator_names():
    return ['*', '/', '%', '+', '-', '..<', '...']
    
def all_arithmetic_comparison_operator_names():
    return ['<', '<=', '>', '>=', '==', '!=']
    
def all_integer_assignment_operator_names():
    return ['<<=', '>>=', '&=', '^=', '|=']
    
def all_integer_or_real_assignment_operator_names():
    return ['=', '*=', '/=', '%=', '+=', '-=']
    
