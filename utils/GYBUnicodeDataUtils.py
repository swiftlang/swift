# ===--- GYBUnicodeDataUtils.py ----------------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import codecs
import re


class UnicodeProperty(object):
    """Abstract base class for Unicode properties."""

    def __init__(self):
        raise NotImplementedError(
            "UnicodeProperty.__init__ is not implemented.")

    def get_default_value(self):
        raise NotImplementedError(
            "UnicodeProperty.get_default_value is not implemented.")

    def get_value(self, cp):
        raise NotImplementedError(
            "UnicodeProperty.get_value is not implemented.")

    def to_numeric_value(self, value):
        raise NotImplementedError(
            "UnicodeProperty.to_numeric_value is not implemented.")

    def get_numeric_value(self, cp):
        raise NotImplementedError(
            "UnicodeProperty.get_numeric_value is not implemented.")


class GraphemeClusterBreakPropertyTable(UnicodeProperty):
    """Grapheme_Cluster_Break property."""

    # An array of tuples (start_code_point, end_code_point, value).
    property_value_ranges = []

    property_values = [None for i in range(0, 0x110000)]

    # Note: Numeric values (including the names) should be consistent with
    # '_GraphemeClusterBreakPropertyValue' enum on the Swift side, and with
    # 'GraphemeClusterBreakProperty' in the compiler C++ code.  If there is a
    # reason for either of those to differ, then this mapping can be overridden
    # after an instance of this class is created.
    numeric_value_table = {
        'Other': 0,
        'CR': 1,
        'LF': 2,
        'Control': 3,
        'Extend': 4,
        'Regional_Indicator': 5,
        'Prepend': 6,
        'SpacingMark': 7,
        'L': 8,
        'V': 9,
        'T': 10,
        'LV': 11,
        'LVT': 12,
    }

    def __init__(self, grapheme_break_property_file_name):
        # Build 'self.symbolic_values' -- an array that maps numeric property
        # values to symbolic values.
        self.symbolic_values = \
            [None] * (max(self.numeric_value_table.values()) + 1)
        for k, v in self.numeric_value_table.items():
            self.symbolic_values[v] = k

        # Load the data file.
        with codecs.open(
                grapheme_break_property_file_name,
                encoding='utf-8',
                errors='strict') as f:
            for line in f:
                # Strip comments.
                line = re.sub('#.*', '', line)

                # Single code point?
                m = re.match('([0-9A-F]+) +; +([a-zA-Z]+) ', line)
                if m:
                    code_point = int(m.group(1), 16)
                    value = m.group(2)
                    self.property_value_ranges += \
                        [(code_point, code_point, value)]
                    continue

                # Range of code points?
                m = re.match(
                    '([0-9A-F]+)..([0-9A-F]+) +; +([a-zA-Z_]+) ', line)
                if m:
                    start_code_point = int(m.group(1), 16)
                    end_code_point = int(m.group(2), 16)
                    value = m.group(3)
                    self.property_value_ranges += \
                        [(start_code_point, end_code_point, value)]

        # Prepare a flat lookup table for fast access.
        for cp in range(0, 0x110000):
            self.property_values[cp] = self.get_default_value()

        for start_code_pt, end_code_pt, val in self.property_value_ranges:
            for cp in range(start_code_pt, end_code_pt + 1):
                self.property_values[cp] = val

    def get_default_value(self):
        return 'Other'

    def get_value(self, cp):
        return self.property_values[cp]

    def to_numeric_value(self, value):
        return self.numeric_value_table[value]

    def get_numeric_value(self, cp):
        return self.to_numeric_value(self.get_value(cp))


# BMP code points are 16-bit values.  The code point value is split as
# follows:
#
#   8 bits                     8 bits
# +-------------------------+-------------------------+
# | 15 14 13 12 11 10  9  8 |  7  6  5  4  3  2  1  0 |
# +-------------------------+-------------------------+
#   first-level index          data offset
#
# Supplementary code points (U+XXXX where XXXX > 0xffff) are 21-bit values.
# The code point value is split as follows:
#
#   5 bits           8 bits                     8 bits
# +----------------+-------------------------+-------------------------+
# | 20 19 18 17 16 | 15 14 13 12 11 10  9  8 |  7  6  5  4  3  2  1  0 |
# +----------------+-------------------------+-------------------------+
#  first-level       second-level index         data offset
#  index
#
# The actual number of bits are just trie parameters.  They affect the size of
# the lookup tables (and thus, lookup time), but do not change the overall
# structure of the trie.
#
# Here and below 'supp' stands for 'supplementary characters'.
#
# Property data for BMP code points is stored as a one-stage trie.
# A trie with one lookup table consists of two memory blocks:
#
#         First-level lookup table
#  +-----+-----+-----+-----+--...--+
#  |  *  |  *  |  *  |  *  |       |
#  +--|--+--|--+--|--+--|--+--...--+
#     |     |     |      \          The references don't form
#     |      \____|       \___,        a systematic pattern
#     |           |           |
#     |           |           |     Data storage
#   +-V--------++-V--------++-V--------++---...---+
#   | data     || data     || data     ||         |
#   +----------++----------++----------++---...---+
#
# In order to fetch data for a given code point, you need to:
# * load from the first-level lookup table using first-level index; this will
#   give you the number of the data block that you should use.
# * load from the data block applying the data offset.
#
# Property data for supplementary code points is stored as a two-stage trie.
# A trie with two-stage lookup tables consists of three memory blocks.  The
# following drawing explains how it is implemented:
#
#         First-level lookup table
#       +-----+-----+-----+-----+-----+--...--+
#       |  *  |  *  |  *  |  *  |  *  |       |
#       +--|--+--|--+--|--+--|--+--|--+--...--+
#          |     |     |     |      \          The references don't form
#      ,__/      |      \____|       \___,        a systematic pattern
#     /          |           |           |
#    |           |           |           | Second-level lookup table
#  +-V--------++-V--------++-V--------++-V--------++---...---+
#  | ******** || ******** || ******** ||          ||         |
#  +-||||||||-++-||||||||-++-||||||||-++----------++---...---+
#    \\\|////    ||||||VV    |VVV|V|V
#     \\|///     ||||||     /    | |
#      \|//      ||||||    /     | |
#       |/       ||||| \__|___.   \ \       The references don't form
#       |        |||| \___|__. \   | \         a systematic pattern
#       |        ||| \____|   \ \__|  \
#       |        || \_____|__. \___|___\       ...___.
#       |        | \______|   \____|    \___,        |  Data storage
#     +-V-----++-V-----++-V-----++-V-----++-V-----++-V-----++---...---+
#     | data  || data  || data  || data  ||       ||       ||         |
#     +-------++-------++-------++-------++-------++-------++---...---+
#
# In order to fetch data for a given code point, you need to:
# * load from the first-level lookup table using first-level index; this will
#   give you the number of the second-level lookup table that you should use.
# * load from the chosen second-level lookup table using the second-level
#   index, which will give you the number of the data block that you should
#   use.
# * load from the data block applying the data offset.
#
# First- and second-level lookup tables in the general case contain 16-bit
# words; that will be sufficient to store a trie that does not compress at all.
# But in many cases, after trie compression there will be fewer than 256
# unique second-level lookup tables and/or data storage blocks, which allows
# one to use 8-bit words in lookup tables.
#
# The bitwidth of data depends on the application of the trie.
#
# The supp tables contain entries for BMP code units to simplify trie
# implementation, but those BMP entries are filled with the default value, so
# they compress well.
class UnicodeTrieGenerator(object):
    # Note: if you change any of these parameters, don't forget to update the
    # ASCII art above.
    bmp_first_level_index_bits = 8

    supp_first_level_index_bits = 5
    supp_second_level_index_bits = 8

    def get_bmp_first_level_index(self, cp):
        return cp >> self.bmp_data_offset_bits

    def get_bmp_data_offset(self, cp):
        return cp & ((1 << self.bmp_data_offset_bits) - 1)

    def get_supp_first_level_index(self, cp):
        return cp >> \
            (self.supp_second_level_index_bits + self.supp_data_offset_bits)

    def get_supp_second_level_index(self, cp):
        return (cp >> self.supp_data_offset_bits) & \
            ((1 << self.supp_second_level_index_bits) - 1)

    def get_supp_data_offset(self, cp):
        return cp & ((1 << self.supp_data_offset_bits) - 1)

    def __init__(self):
        """Create a trie generator with default parameters."""
        pass

    def create_tables(self):
        """Compute derived parameter values and create internal data
        structures.

        Don't change parameter values after calling this method.
        """
        self.bmp_data_offset_bits = 16 - self.bmp_first_level_index_bits

        self.supp_data_offset_bits = \
            21 - self.supp_first_level_index_bits - \
            self.supp_second_level_index_bits

        # The maximum value of the first level index for supp tables.  It is
        # not equal to ((1 << supp_first_level_index_bits) - 1), because
        # maximum Unicode code point value is not 2^21-1 (0x1fffff), it is
        # 0x10ffff.
        self.supp_first_level_index_max = \
            0x10ffff >> \
            (self.supp_second_level_index_bits + self.supp_data_offset_bits)

        # A mapping from BMP first-level index to BMP data block index.
        self.bmp_lookup = \
            [i for i in range(0, 1 << self.bmp_first_level_index_bits)]

        # An array of BMP data blocks.
        self.bmp_data = [
            [-1 for i in range(0, 1 << self.bmp_data_offset_bits)]
            for i in range(0, 1 << self.bmp_first_level_index_bits)
        ]

        # A mapping from supp first-level index to an index of the second-level
        # lookup table.
        self.supp_lookup1 = \
            [i for i in range(0, self.supp_first_level_index_max + 1)]

        # An array of second-level lookup tables.  Each second-level lookup
        # table is a mapping from a supp second-level index to supp data block
        # index.
        self.supp_lookup2 = [
            [j for j in range(i << self.supp_second_level_index_bits,
                              (i + 1) << self.supp_second_level_index_bits)]
            for i in range(0, self.supp_first_level_index_max + 1)
        ]

        # An array of supp data blocks.
        self.supp_data = [
            [-1 for i in range(0, 1 << self.supp_data_offset_bits)]
            for i in range(0, (self.supp_first_level_index_max + 1) *
                           (1 << self.supp_second_level_index_bits))
        ]

    def splat(self, value):
        for i in range(0, len(self.bmp_data)):
            for j in range(0, len(self.bmp_data[i])):
                self.bmp_data[i][j] = value

        for i in range(0, len(self.supp_data)):
            for j in range(0, len(self.supp_data[i])):
                self.supp_data[i][j] = value

    def set_value(self, cp, value):
        if cp <= 0xffff:
            data_block_index = self.bmp_lookup[
                self.get_bmp_first_level_index(cp)]
            self.bmp_data[data_block_index][
                self.get_bmp_data_offset(cp)] = value
        else:
            second_lookup_index = self.supp_lookup1[
                self.get_supp_first_level_index(cp)]
            data_block_index = self.supp_lookup2[second_lookup_index][
                self.get_supp_second_level_index(cp)]
            self.supp_data[data_block_index][
                self.get_supp_data_offset(cp)] = value

    def get_value(self, cp):
        if cp <= 0xffff:
            data_block_index = self.bmp_lookup[
                self.get_bmp_first_level_index(cp)]
            return self.bmp_data[data_block_index][
                self.get_bmp_data_offset(cp)]
        else:
            second_lookup_index = self.supp_lookup1[
                self.get_supp_first_level_index(cp)]
            data_block_index = self.supp_lookup2[second_lookup_index][
                self.get_supp_second_level_index(cp)]
            return self.supp_data[data_block_index][
                self.get_supp_data_offset(cp)]

    def fill_from_unicode_property(self, unicode_property):
        self.splat(unicode_property.get_default_value())
        for cp in range(0, 0x110000):
            self.set_value(cp, unicode_property.get_value(cp))

    def verify(self, unicode_property):
        for cp in range(0, 0x110000):
            expected_value = unicode_property.get_value(cp)
            actual_value = self.get_value(cp)
            assert(expected_value == actual_value)

    def freeze(self):
        """Compress internal trie representation.

        Don't mutate the trie after calling this method.
        """
        def remap_indexes(indexes, old_idx, new_idx):
            def map_index(idx):
                if idx == old_idx:
                    return new_idx
                elif idx > old_idx:
                    return idx - 1
                else:
                    return idx

            # NOTE: Python 2's `map` function returns a list. Where Python 3's
            # `map` function returns an iterator. To work around this the
            # result of the `map` is explicitly converted to a `list`.
            return list(map(map_index, indexes))

        # If self.bmp_data contains identical data blocks, keep the first one,
        # remove duplicates and change the indexes in self.bmp_lookup to point
        # to the first one.
        i = 0
        while i < len(self.bmp_data):
            j = i + 1
            while j < len(self.bmp_data):
                if self.bmp_data[i] == self.bmp_data[j]:
                    self.bmp_data.pop(j)
                    self.bmp_lookup = \
                        remap_indexes(self.bmp_lookup, old_idx=j, new_idx=i)
                else:
                    j += 1
            i += 1

        # For supp tables, perform bottom-up deduplication: first, deduplicate
        # data blocks.  The algorithm is the same as above, but operates on
        # self.supp_data/supp_lookup2.
        i = 0
        while i < len(self.supp_data):
            j = i + 1
            while j < len(self.supp_data):
                if self.supp_data[i] == self.supp_data[j]:
                    self.supp_data.pop(j)
                    for k in range(0, len(self.supp_lookup2)):
                        self.supp_lookup2[k] = \
                            remap_indexes(self.supp_lookup2[k],
                                          old_idx=j, new_idx=i)
                else:
                    j += 1
            i += 1

        # Next, deduplicate second-level lookup tables.
        # Same as above, but for supp_lookup1/supp_lookup2.
        i = 0
        while i < len(self.supp_lookup2):
            j = i + 1
            while j < len(self.supp_lookup2):
                if self.supp_lookup2[i] == self.supp_lookup2[j]:
                    self.supp_lookup2.pop(j)
                    self.supp_lookup1 = \
                        remap_indexes(self.supp_lookup1, old_idx=j, new_idx=i)
                else:
                    j += 1
            i += 1

    def _int_to_le_bytes(self, data, width):
        if width == 1:
            assert(data & ~0xff == 0)
            return [data]
        if width == 2:
            assert(data & ~0xffff == 0)
            return [data & 0xff, data & 0xff00]
        assert(False)

    def _int_list_to_le_bytes(self, ints, width):
        return [
            byte
            for elt in ints
            for byte in self._int_to_le_bytes(elt, width)]

    def serialize(self, unicode_property):
        self.bmp_lookup_bytes_per_entry = 1 if len(self.bmp_data) < 256 else 2
        self.bmp_data_bytes_per_entry = 1

        self.supp_lookup1_bytes_per_entry = 1 if len(self.supp_lookup2) < 256 \
            else 2
        self.supp_lookup2_bytes_per_entry = 1 if len(self.supp_data) < 256 \
            else 2
        self.supp_data_bytes_per_entry = 1

        bmp_lookup_words = list(self.bmp_lookup)
        bmp_data_words = [
            unicode_property.to_numeric_value(elt)
            for block in self.bmp_data
            for elt in block]

        supp_lookup1_words = list(self.supp_lookup1)
        supp_lookup2_words = [
            elt for block in self.supp_lookup2 for elt in block]
        supp_data_words = [
            unicode_property.to_numeric_value(elt)
            for block in self.supp_data
            for elt in block]

        bmp_lookup_bytes = self._int_list_to_le_bytes(
            bmp_lookup_words, self.bmp_lookup_bytes_per_entry)
        bmp_data_bytes = self._int_list_to_le_bytes(
            bmp_data_words, self.bmp_data_bytes_per_entry)

        supp_lookup1_bytes = self._int_list_to_le_bytes(
            supp_lookup1_words, self.supp_lookup1_bytes_per_entry)
        supp_lookup2_bytes = self._int_list_to_le_bytes(
            supp_lookup2_words, self.supp_lookup2_bytes_per_entry)
        supp_data_bytes = self._int_list_to_le_bytes(
            supp_data_words, self.supp_data_bytes_per_entry)

        self.trie_bytes = []

        self.bmp_lookup_bytes_offset = 0
        self.trie_bytes += bmp_lookup_bytes

        self.bmp_data_bytes_offset = len(self.trie_bytes)
        self.trie_bytes += bmp_data_bytes

        self.supp_lookup1_bytes_offset = len(self.trie_bytes)
        self.trie_bytes += supp_lookup1_bytes

        self.supp_lookup2_bytes_offset = len(self.trie_bytes)
        self.trie_bytes += supp_lookup2_bytes

        self.supp_data_bytes_offset = len(self.trie_bytes)
        self.trie_bytes += supp_data_bytes


def get_extended_grapheme_cluster_rules_matrix(grapheme_cluster_break_table):
    any_value = \
        grapheme_cluster_break_table.symbolic_values

    # Rules to determine extended grapheme cluster boundaries, as defined in
    # 'Grapheme Break Chart',
    # http://www.unicode.org/Public/6.3.0/ucd/auxiliary/GraphemeBreakTest.html,
    # Unicode 6.3.0.
    #
    # The Unicode 7.0.0 draft does not change these rules.
    #
    # As in the referenced document, the rules are specified in order of
    # decreasing priority.
    rules = [
        (['CR'], 'no_boundary', ['LF']),
        (['Control', 'CR', 'LF'], 'boundary', any_value),
        (any_value, 'boundary', ['Control', 'CR', 'LF']),
        (['L'], 'no_boundary', ['L', 'V', 'LV', 'LVT']),
        (['LV', 'V'], 'no_boundary', ['V', 'T']),
        (['LVT', 'T'], 'no_boundary', ['T']),
        (['Regional_Indicator'], 'no_boundary', ['Regional_Indicator']),
        (any_value, 'no_boundary', ['Extend']),
        (any_value, 'no_boundary', ['SpacingMark']),
        (['Prepend'], 'no_boundary', any_value),
        (any_value, 'boundary', any_value),
    ]

    # Expand the rules into a matrix.
    rules_matrix = {}
    for first in any_value:
        rules_matrix[first] = \
            dict.fromkeys(any_value, None)

    # Iterate over rules in the order of increasing priority.
    for first_list, action, second_list in reversed(rules):
        for first in first_list:
            for second in second_list:
                rules_matrix[first][second] = action

    # Make sure we can pack one row of the matrix into a 'uint16_t'.
    assert(len(any_value) <= 16)

    result = []
    for first in any_value:
        # Retrieve a row that corresponds to this first code point.
        row = rules_matrix[first]

        # Change strings into bits.
        bits = [row[second] == 'no_boundary' for second in any_value]

        # Pack bits into an integer.
        packed = sum([bits[i] * pow(2, i) for i in range(0, len(bits))])

        result += [packed]

    return result


def get_grapheme_cluster_break_tests_as_utf8(grapheme_break_test_file_name):
    def _convert_line(line):
        # Strip comments.
        line = re.sub('#.*', '', line).strip()

        if line == "":
            return None

        test = ""
        curr_bytes = 0
        boundaries = []

        # Match a list of code points.
        for token in line.split(" "):
            if token == u"÷":
                boundaries += [curr_bytes]
            elif token == u"×":
                pass
            else:
                code_point = int(token, 16)
                # Tests from Unicode spec have isolated surrogates in them.
                # Our segmentation algorithm works on UTF-8 sequences, so
                # encoding a surrogate would produce an invalid code unit
                # sequence. Instead of trying to emulate the maximal subpart
                # algorithm for inserting U+FFFD in Python, we just replace
                # every isolated surrogate with U+200B, which also has
                # Grapheme_Cluster_Break equal to 'Control' and test
                # separately that we handle ill-formed UTF-8 sequences.
                if code_point >= 0xd800 and code_point <= 0xdfff:
                    code_point = 0x200b
                code_point = (b'\U%(cp)08x' % {b'cp': code_point}).decode(
                    'unicode_escape', 'strict')
                as_utf8_bytes = bytearray(code_point.encode('utf8', 'strict'))
                as_utf8_escaped = ''.join(
                    ['\\x%(byte)02x' % {'byte': byte}
                     for byte in as_utf8_bytes])
                test += as_utf8_escaped
                curr_bytes += len(as_utf8_bytes)

        return (test, boundaries)

    # Self-test.
    assert(_convert_line(u'÷ 0903 × 0308 ÷ AC01 ÷ # abc') == (
        '\\xe0\\xa4\\x83\\xcc\\x88\\xea\\xb0\\x81', [0, 5, 8]))
    assert(_convert_line(u'÷ D800 ÷ # abc') == ('\\xe2\\x80\\x8b', [0, 3]))

    result = []

    with codecs.open(
            grapheme_break_test_file_name,
            encoding='utf-8',
            errors='strict') as f:
        for line in f:
            test = _convert_line(line)
            if test:
                result += [test]

    return result


def get_grapheme_cluster_break_tests_as_unicode_scalars(
        grapheme_break_test_file_name):
    def _convert_line(line):
        # Strip comments.
        line = re.sub('#.*', '', line).strip()

        if line == "":
            return None

        test = []
        curr_code_points = 0
        boundaries = []

        # Match a list of code points.
        for token in line.split(" "):
            if token == "÷":
                boundaries += [curr_code_points]
            elif token == "×":
                pass
            else:
                code_point = int(token, 16)
                # Tests from Unicode spec have isolated surrogates in them. Our
                # segmentation algorithm works on UTF-16 sequences, so encoding
                # a surrogate would produce an invalid code unit sequence.
                # Instead of trying to emulate the maximal subpart algorithm
                # for inserting U+FFFD in Python, we just replace every
                # isolated surrogate with U+200B, which also has
                # Grapheme_Cluster_Break equal to 'Control' and test separately
                # that we handle ill-formed UTF-8 sequences.
                if code_point >= 0xd800 and code_point <= 0xdfff:
                    code_point = 0x200b
                test += [code_point]
                curr_code_points += 1

        return (test, boundaries)

    # Self-test.
    assert(_convert_line('÷ 0903 × 0308 ÷ AC01 ÷ # abc') == ([
        0x0903, 0x0308, 0xac01], [0, 2, 3]))
    assert(_convert_line('÷ D800 ÷ # abc') == ([0x200b], [0, 1]))

    result = []

    with open(grapheme_break_test_file_name, 'rb') as f:
        for line in f:
            test = _convert_line(line)
            if test:
                result += [test]

    return result
