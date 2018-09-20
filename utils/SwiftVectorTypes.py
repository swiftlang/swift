# ===--- SwiftVectorTypes.py --------------------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from SwiftIntTypes import all_integer_types
all_vector_counts = [2,3,4,8,16,32,64]
vector_max_bits = 512

class SwiftVectorType(object):
  def __init__(self, element, count):
    self.element = element
    self.count = count
    self.short_name = 'Vector' + str(count)
    self.stdlib_name = element.stdlib_name + '.' + self.short_name
    self.builtin_name = 'Vec' + str(count if count != 3 else 4) + 'x' + element.builtin_name

  def get_signed_integer_type(self):
    return SwiftVectorType(SwiftIntegerType(false, self.element.bits, true), count)

def all_vector_types(element):
  return [SwiftVectorType(element,c) for c in all_vector_counts if c*element.bits <= vector_max_bits]
