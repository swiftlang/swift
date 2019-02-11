#!/usr/bin/env python

import argparse
import re
import subprocess
import sys


def main(arguments):
    parser = argparse.ArgumentParser(
        description='Analyze the code size in a binary')
    parser.add_argument('-arch', type=str,
                        help='the arch to look at', default='arm64')
    parser.add_argument('-categorize', action='store_true',
                        help='categorize symbols', dest='build_categories',
                        default=False)
    parser.add_argument('-list-category', type=str,
                        help='list symbols in category')
    parser.add_argument('-uncategorized', action='store_true',
                        help='show all uncategorized symbols',
                        dest='show_uncategorized',
                        default=False)
    parser.add_argument('bin', help='the binary')

    args = parser.parse_args(arguments)

    segments = parse_segments(args.bin, args.arch)

    if args.build_categories:
        categorize(segments)
    elif args.show_uncategorized:
        uncategorized(segments)
    elif args.list_category:
        list_category(segments, args.list_category)
    else:
        show_all(segments)


class Symbol(object):
    def __init__(self, name, mangled_name, size):
        self.name = name
        self.mangled_name = mangled_name
        self.count = 1
        self.size = int(size)


def get_symbol_size(sym):
    return sym.size


class Segment(object):
    def __init__(self, name):
        self.name = name
        self.sections = []


class Section(object):
    def __init__(self, name, size):
        self.name = name
        self.size = size
        self.symbols = []


class Category(object):
    def __init__(self, name):
        self.name = name
        self.size = 0
        self.symbols = []

    def add(self, symbol):
        self.symbols.append(symbol)
        self.size += symbol.size


class Categories(object):
    def __init__(self):
        self.category_matching = [
            ['Objective-C function', re.compile(r'.*[+-]\[')],
            ['C++', re.compile(r'_+swift')],
            ['Merged function', re.compile(r'merged ')],
            ['Key path', re.compile(r'key path')],
            ['Function signature specialization',
                re.compile(r'function signature specialization')],
            ['Generic specialization', re.compile(r'generic specialization')],
            ['Reabstraction thunk helper',
                re.compile(r'reabstraction thunk helper')],
            ['vtable thunk', re.compile(r'vtable thunk for')],
            ['@objc thunk', re.compile(r'@objc')],
            ['@nonobjc thunk', re.compile(r'@nonobjc')],
            ['Value witness', re.compile(r'.*value witness for')],
            ['Block copy helper', re.compile(r'_block_copy_helper')],
            ['Block destroy helper', re.compile(r'_block_destroy_helper')],
            ['Block literal global', re.compile(r'___block_literal_global')],
            ['Destroy helper block', re.compile(r'___destroy_helper_block')],
            ['Copy helper block', re.compile(r'___copy_helper_block')],
            ['Object destroy', re.compile(r'_objectdestroy')],
            ['Partial apply forwarder',
                re.compile(r'partial apply forwarder')],
            ['Closure function', re.compile(r'closure #')],
            ['ObjC metadata update function',
                re.compile(r'ObjC metadata update function for')],
            ['Variable initialization expression',
                re.compile(r'variable initialization expression of')],
            ['Global initialization', re.compile(r'_globalinit_')],
            ['Unnamed', re.compile(r'___unnamed_')],
            ['Dyld stubs', re.compile(r'DYLD-STUB\$')],
            ['Witness table accessor',
                re.compile(r'.*witness table accessor for')],
            ['Protocol witness', re.compile(r'protocol witness for')],
            ['Outlined variable', re.compile(r'outlined variable #')],
            ['Outlined value function (copy,destroy,release...)',
                re.compile(r'outlined')],
            ['_symbolic', re.compile(r'_symbolic')],
            ['_associated conformance',
                re.compile(r'_associated conformance')],
            ['Direct field offset', re.compile(r'direct field offset for')],
            ['Value witness tables', re.compile(r'.*value witness table')],
            ['Protocol witness table',
                re.compile(r'.*protocol witness table for')],
            ['Protocol conformance descriptor',
                re.compile(r'protocol conformance descriptor for')],
            ['Lazy protocol witness table cache var',
                re.compile(
                    r'lazy protocol witness table cache variable for type')],
            ['Nominal type descriptor',
                re.compile(r'nominal type descriptor for')],
            ['ObjC class', re.compile(r'_OBJC_CLASS_')],
            ['ObjC metaclass', re.compile(r'_OBJC_METACLASS')],
            ['ObjC ivar', re.compile(r'_OBJC_IVAR')],
            ['Metaclass', re.compile(r'metaclass for')],
            ['Block descriptor', re.compile(r'_+block_descriptor')],
            ['Extension descriptor', re.compile(r'extension descriptor')],
            ['Module descriptor', re.compile(r'module descriptor')],
            ['Associated type descriptor',
                re.compile(r'associated type descriptor for')],
            ['Associated conformance descriptor',
                re.compile(r'associated conformance descriptor for')],
            ['Protocol descriptor', re.compile(r'protocol descriptor for')],
            ['Base conformance descriptor',
                re.compile(r'base conformance descriptor for')],
            ['Protocol requirements base descriptor',
                re.compile(r'protocol requirements base descriptor for')],
            ['Property descriptor', re.compile(r'property descriptor for')],
            ['Method descriptor', re.compile(r'method descriptor for')],
            ['Anonymous descriptor', re.compile(r'anonymous descriptor')],
            ['Type metadata accessor',
                re.compile(r'.*type metadata accessor')],
            ['Type metadata', re.compile(r'.*type metadata')],
            ['Reflection metadata descriptor',
                re.compile(r'reflection metadata .* descriptor')],
        ]

        self.category_mangled_matching = [
            ['Swift variable storage', re.compile(r'^_\$s.*[v][p][Z]?$')],
            ['Swift constructor', re.compile(r'^_\$s.*[f][cC]$')],
            ['Swift initializer', re.compile(r'^_\$s.*[f][ie]$')],
            ['Swift destructor/destroyer', re.compile(r'^_\$s.*[f][dDE]$')],
            ['Swift getter', re.compile(r'^_\$s.*[iv][gG]$')],
            ['Swift setter', re.compile(r'^_\$s.*[iv][swW]$')],
            ['Swift materializeForSet', re.compile(r'^_\$s.*[iv][m]$')],
            ['Swift modify', re.compile(r'^_\$s.*[iv][M]$')],
            ['Swift read', re.compile(r'^_\$s.*[iv][r]$')],
            ['Swift addressor', re.compile(r'^_\$s.*[iv][al][uOop]$')],
            ['Swift function', re.compile(r'^_\$s.*F$')],
            ['Swift unknown', re.compile(r'^_\$s.*')],
        ]
        self.categories = {}

    def categorize_by_name(self, symbol):
        for c in self.category_matching:
            if c[1].match(symbol.name):
                return c[0]
        return None

    def categorize_by_mangled_name(self, symbol):
        for c in self.category_mangled_matching:
            if c[1].match(symbol.mangled_name):
                return c[0]
        return None

    def add_symbol(self, category_name, symbol):
        existing_category = self.categories.get(category_name)
        if existing_category:
            existing_category.add(symbol)
        else:
            new_category = Category(category_name)
            new_category.add(symbol)
            self.categories[category_name] = new_category

    def add(self, symbol):
        category_name = self.categorize_by_name(symbol)
        if category_name:
            self.add_symbol(category_name, symbol)
            return
        category_name = self.categorize_by_mangled_name(symbol)
        if category_name:
            self.add_symbol(category_name, symbol)
        else:
            self.add_symbol('Unknown', symbol)

    def categorize(self, symbols):
        for sym in symbols:
            self.add(sym)

    def print_summary(self, section_size):
        names = [c[0] for c in self.category_matching]
        names.extend([c[0] for c in self.category_mangled_matching])
        names.append('Unknown')
        total_size = 0
        for name in names:
            category = self.categories.get(name)
            size = 0
            if category:
                size = category.size
            total_size += size
            if size > 0:
                print("%60s: %8d (%6.2f%%)" %
                      (name, size, (float(size) * 100) / section_size))
        print("%60s: %8d (%6.2f%%)" % ('TOTAL', total_size, float(100)))

    def uncatorizedSymbols(self):
        category = self.categories.get('Unknown')
        if category:
            return category.symbols
        return None

    def print_uncategorizedSymbols(self):
        syms = self.uncatorizedSymbols()
        if syms:
            for symbol in syms:
                print(symbol.mangled_name + " " + symbol.name + " " +
                      str(symbol.size))

    def print_category(self, category):
        category = self.categories.get(category)
        if category:
            if category.symbols:
                sorted_symbols = sorted(category.symbols, key=get_symbol_size)
                for sym in sorted_symbols:
                    print('%8d %s %s' % (sym.size, sym.name, sym.mangled_name))

    def has_category(self, category):
        category = self.categories.get(category)
        if category:
            if category.symbols:
                return True
        return False


def parse_segments(path, arch):
    mangled = subprocess.check_output(
        ['symbols', '-noSources', '-noDemangling', '-arch', arch, path])
    demangle = subprocess.Popen(
        ['xcrun', 'swift-demangle'], stdin=subprocess.PIPE,
        stdout=subprocess.PIPE)
    demangled = demangle.communicate(mangled)[0]
    symbols = {}
    segments = []
    segment_regex = re.compile(
        r"^        0x[0-9a-f]+ \(\s*0x(?P<size>[0-9a-f]+)\) "
        r"(?P<name>.+?) (?P<name2>.+?)$")
    section_regex = re.compile(
        r"^            0x[0-9a-f]+ \(\s*0x(?P<size>[0-9a-f]+)\) "
        r"(?P<name>.+?) (?P<name2>.+?)$")
    symbol_regex = re.compile(
        r"^                0x[0-9a-f]+ \(\s*0x(?P<size>[0-9a-f]+)\) "
        r"(?P<name>.+?) \[[^\]]+\] $")

    mangled_lines = mangled.splitlines()
    current_line_number = 0

    for line in demangled.splitlines():
        mangled_line = mangled_lines[current_line_number]
        current_line_number += 1

        # Match a segment entry.
        segment_match = segment_regex.match(line)
        if segment_match:
            new_segment = Segment(segment_match.group('name'))
            segments.append(new_segment)
            continue

        # Match a section entry.
        section_match = section_regex.match(line)
        if section_match:
            new_section = Section(section_match.group('name2'),
                                  int(section_match.group('size'), 16))
            segments[-1].sections.append(new_section)
            continue

        # Match a symbol entry.
        symbol_match = symbol_regex.match(line)
        if not symbol_match:
            continue
        mangled_symbol_match = symbol_regex.match(mangled_line)
        if not mangled_symbol_match:
            print('mangled and demangled mismatch')
            print(mangled_line)
            print(line)
            assert False

        symbol = Symbol(symbol_match.group('name'),
                        mangled_symbol_match.group('name'),
                        int(symbol_match.group('size'), 16))
        existing = symbols.get(symbol.name)
        if existing:
            existing.size += symbol.size
        else:
            symbols[symbol.name] = symbol
            segments[-1].sections[-1].symbols.append(symbol)

    return segments


def show_all(segments):
    for segment in segments:
        for section in segment.sections:
            symbols = section.symbols
            for sym in symbols:
                print(str(sym.size) + ' ' + sym.name + ' ' + sym.mangled_name)


def categorize(segments):
    for segment in segments:
        for section in segment.sections:
            print('Section %52s: %8d' %
                  (segment.name + ';' + section.name, section.size))
            symbols = section.symbols
            categories = Categories()
            categories.categorize(symbols)
            categories.print_summary(section.size)
            print('')


def uncategorized(segments):
    for segment in segments:
        for section in segment.sections:
            symbols = section.symbols
            categories = Categories()
            categories.categorize(symbols)
            categories.print_uncategorizedSymbols()


def list_category(segments, category):
    for segment in segments:
        for section in segment.sections:
            symbols = section.symbols
            categories = Categories()
            categories.categorize(symbols)
            if categories.has_category(category):
                print('Section %22s: %8d' %
                      (segment.name + ';' + section.name, section.size))
                categories.print_category(category)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
