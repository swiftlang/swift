#!/usr/bin/env python
# line-directive.py - Transform line numbers in error messages -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# Converts line numbers in error messages according to "line directive"
# comments.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import bisect
import os
import re
import shlex
import subprocess
import sys

line_pattern = re.compile(
    r'^// ###sourceLocation\(file:\s*"([^"]+)",\s*line:\s*([0-9]+)\s*\)')


def _make_line_map(target_filename, stream=None):
    """
    >>> from StringIO import StringIO
    >>> _make_line_map('box',
    ... StringIO('''// ###sourceLocation(file: "foo.bar", line: 3)
    ... line 2
    ... line 3
    ... line 4
    ... // ###sourceLocation(file: "baz.txt", line: 20)
    ... line 6
    ... line 7
    ... '''))
    [(0, 'box', 1), (1, 'foo.bar', 3), (5, 'baz.txt', 20)]
    """
    result = [(0, target_filename, 1)]
    input = stream or open(target_filename)
    for i, l in enumerate(input.readlines()):
        m = line_pattern.match(l)
        if m:
            result.append((i + 1, m.group(1), int(m.group(2))))
    return result


_line_maps = {}


def fline_map(target_filename):
    map = _line_maps.get(target_filename)
    if map is None:
        map = _make_line_map(target_filename)
        _line_maps[target_filename] = map
    return map


def map_line_to_source_file(target_filename, target_line_num):
    """
    >>> from tempfile import *
    >>> # On Windows, the name of a NamedTemporaryFile cannot be used to open
    >>> # the file for a second time if delete=True. Therefore, we have to
    >>> # manually handle closing and deleting this file to allow us to open
    >>> # the file by its name across all platforms.
    >>> t = NamedTemporaryFile(delete=False)
    >>> t.write('''line 1
    ... line 2
    ... // ###sourceLocation(file: "foo.bar", line: 20)
    ... line 4
    ... line 5
    ... // ###sourceLocation(file: "baz.txt", line: 5)
    ... line 7
    ... line 8
    ... ''')
    >>> t.flush()
    >>> (t2, l) = map_line_to_source_file(t.name, 1)
    >>> t2 == t.name, l
    (True, 1)
    >>> (t2, l) = map_line_to_source_file(t.name, 2)
    >>> t2 == t.name, l
    (True, 2)
    >>> (t2, l) = map_line_to_source_file(t.name, 3)
    >>> t2 == t.name, l
    (True, 3)
    >>> map_line_to_source_file(t.name, 4)
    ('foo.bar', 20)
    >>> map_line_to_source_file(t.name, 5)
    ('foo.bar', 21)
    >>> map_line_to_source_file(t.name, 6)
    ('foo.bar', 22)
    >>> map_line_to_source_file(t.name, 7)
    ('baz.txt', 5)
    >>> map_line_to_source_file(t.name, 8)
    ('baz.txt', 6)
    >>> map_line_to_source_file(t.name, 42)
    ('baz.txt', 40)
    >>> t.close()
    >>> os.remove(t.name)
    """
    assert(target_line_num > 0)
    map = fline_map(target_filename)
    index = bisect.bisect_left(map, (target_line_num, '', 0))
    base = map[index - 1]
    return base[1], base[2] + (target_line_num - base[0] - 1)


def map_line_from_source_file(source_filename, source_line_num,
                              target_filename):
    """
    >>> from tempfile import *
    >>> # On Windows, the name of a NamedTemporaryFile cannot be used to open
    >>> # the file for a second time if delete=True. Therefore, we have to
    >>> # manually handle closing and deleting this file to allow us to open
    >>> # the file by its name across all platforms.
    >>> t = NamedTemporaryFile(delete=False)
    >>> t.write('''line 1
    ... line 2
    ... // ###sourceLocation(file: "foo.bar", line: 20)
    ... line 4
    ... line 5
    ... // ###sourceLocation(file: "baz.txt", line: 5)
    ... line 7
    ... line 8
    ... ''')
    >>> t.flush()
    >>> map_line_from_source_file(t.name, 1, t.name)
    1
    >>> map_line_from_source_file(t.name, 2, t.name)
    2
    >>> map_line_from_source_file(t.name, 3, t.name)
    3
    >>> try: map_line_from_source_file(t.name, 4, t.name)
    ... except RuntimeError: pass
    >>> try: map_line_from_source_file('foo.bar', 19, t.name)
    ... except RuntimeError: pass
    >>> map_line_from_source_file('foo.bar', 20, t.name)
    4
    >>> map_line_from_source_file('foo.bar', 21, t.name)
    5
    >>> map_line_from_source_file('foo.bar', 22, t.name)
    6
    >>> try: map_line_from_source_file('foo.bar', 23, t.name)
    ... except RuntimeError: pass
    >>> map_line_from_source_file('baz.txt', 5, t.name)
    7
    >>> map_line_from_source_file('baz.txt', 6, t.name)
    8
    >>> map_line_from_source_file('baz.txt', 33, t.name)
    35
    >>> try: map_line_from_source_file(t.name, 33, t.name)
    ... except RuntimeError: pass
    >>> try: map_line_from_source_file('foo.bar', 2, t.name)
    ... except RuntimeError: pass
    >>> t.close()
    >>> os.remove(t.name)
    """
    assert(source_line_num > 0)
    map = fline_map(target_filename)

    for i, (target_line_num, found_source_filename,
            found_source_line_num) in enumerate(map):
        if found_source_filename != source_filename:
            continue
        if found_source_line_num > source_line_num:
            continue
        result = target_line_num + (source_line_num - found_source_line_num)
        if i + 1 == len(map) or map[i + 1][0] > result:
            return result + 1
    raise RuntimeError("line not found")


def read_response_file(file_path):
    with open(file_path, 'r') as files:
        # "Make an iterator out of shlex.shlex.get_token, then consume items
        # until it returns None." (Then eagerly convert the result to a list so
        # that we can close the file.)
        return list(iter(shlex.shlex(files, file_path, posix=True).get_token,
                         None))


def expand_response_files(files):
    expanded_files = []
    for file_path in files:
        # Read a list of files from a response file.
        if file_path[0] == '@':
            expanded_files.extend(read_response_file(file_path[1:]))
        else:
            expanded_files.append(file_path)

    return expanded_files


def run():
    """Simulate a couple of gyb-generated files

    >>> from tempfile import *
    >>> # On Windows, the name of a NamedTemporaryFile cannot be used to open
    >>> # the file for a second time if delete=True. Therefore, we have to
    >>> # manually handle closing and deleting this file to allow us to open
    >>> # the file by its name across all platforms.
    >>> target1 = NamedTemporaryFile(delete=False)
    >>> target1.write('''line 1
    ... line 2
    ... // ###sourceLocation(file: "foo.bar", line: 20)
    ... line 4
    ... line 5
    ... // ###sourceLocation(file: "baz.txt", line: 5)
    ... line 7
    ... line 8
    ... ''')
    >>> target1.flush()
    >>> # On Windows, the name of a NamedTemporaryFile cannot be used to open
    >>> # the file for a second time if delete=True. Therefore, we have to
    >>> # manually handle closing and deleting this file to allow us to open
    >>> # the file by its name across all platforms.
    >>> target2 = NamedTemporaryFile(delete=False)
    >>> target2.write('''// ###sourceLocation(file: "foo.bar", line: 7)
    ... line 2
    ... line 3
    ... // ###sourceLocation(file: "fox.box", line: 11)
    ... line 5
    ... line 6
    ... ''')
    >>> target2.flush()

    Simulate the raw output of compilation

    >>> # On Windows, the name of a NamedTemporaryFile cannot be used to open
    >>> # the file for a second time if delete=True. Therefore, we have to
    >>> # manually handle closing and deleting this file to allow us to open
    >>> # the file by its name across all platforms.
    >>> raw_output = NamedTemporaryFile(delete=False)
    >>> target1_name, target2_name = target1.name, target2.name
    >>> raw_output.write('''A
    ... %(target1_name)s:2:111: error one
    ... B
    ... %(target1_name)s:4:222: error two
    ... C
    ... %(target1_name)s:8:333: error three
    ... D
    ... glitch in file %(target2_name)s:1 assert one
    ... E
    ... glitch in file %(target2_name)s, line 2 assert two
    ... glitch at %(target2_name)s, line 3 assert three
    ... glitch at %(target2_name)s:4 assert four
    ... glitch in [%(target2_name)s, line 5 assert five
    ... glitch in [%(target2_name)s:22 assert six
    ... ''' % locals())
    >>> raw_output.flush()

    Run this tool on the two targets, using a portable version of Unix 'cat' to
    dump the output file.

    >>> import subprocess
    >>> output = subprocess.check_output([sys.executable,
    ...    __file__, target1.name, target2.name, '--',
    ...    sys.executable, '-c',
    ...   'import sys;sys.stdout.write(open(sys.argv[1]).read())',
    ...   raw_output.name], universal_newlines=True)

    Replace temporary filenames and check it.

    >>> print(output.replace(target1.name,'TARGET1-NAME')
    ...             .replace(target2.name,'TARGET2-NAME') + 'EOF')
    A
    TARGET1-NAME:2:111: error one
    B
    foo.bar:20:222: error two
    C
    baz.txt:6:333: error three
    D
    glitch in file TARGET2-NAME:1 assert one
    E
    glitch in file foo.bar, line 7 assert two
    glitch at foo.bar, line 8 assert three
    glitch at foo.bar:9 assert four
    glitch in [fox.box, line 11 assert five
    glitch in [fox.box:28 assert six
    EOF
    >>> print(subprocess.check_output([sys.executable, __file__, 'foo.bar',
    ...                                '21', target1.name],
    ...                               universal_newlines=True), end='')
    5
    >>> print(subprocess.check_output([sys.executable, __file__, 'foo.bar',
    ...                                '8', target2.name],
    ...                               universal_newlines=True), end='')
    3

    Simulate errors on different line numbers
    >>> # On Windows, the name of a NamedTemporaryFile cannot be used to open
    >>> # the file for a second time if delete=True. Therefore, we have to
    >>> # manually handle closing and deleting this file to allow us to open
    >>> # the file by its name across all platforms.
    >>> long_output = NamedTemporaryFile(delete=False)
    >>> long_output.write('''
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 1)
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 1)
    ... //===--- Map.swift.gyb - Lazily map over a Sequence -----------*- swif
    ... //
    ... // This source file is part of the Swift.org open source project
    ... //
    ... // Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
    ... // Licensed under Apache License v2.0 with Runtime Library Exception
    ... //
    ... // See https://swift.org/LICENSE.txt for license information
    ... // See https://swift.org/CONTRIBUTORS.txt for the list of Swift projec
    ... //
    ... //===-----------------------------------------------------------------
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 20)
    ...
    ... /// The `IteratorProtocol` used by `MapSequence` and `MapCollection`.
    ... /// Produces each element by passing the output of the `Base`
    ... /// `IteratorProtocol` through a transform function returning
    ... @_fixed_layout
    ... public struct LazyMapIterator<
    ...   Base : IteratorProtocol, Element
    ... > : IteratorProtocol, Sequence {
    ...   /// Advances to the next element and returns it, or `nil` if no
    ...   /// exists.
    ...   ///
    ...   /// Once `nil` has been returned, all subsequent calls return `nil`.
    ...   ///
    ...   /// - Precondition: `next()` has not been applied to a copy.
    ...   ///   since the copy was made.
    ...   @_inlineable
    ...   public mutating func next() -> Element? {
    ...     return _base.next().map(_transform)
    ...   }
    ...
    ...   @_inlineable
    ...   public var base: Base { return _base }
    ...
    ...   @_versioned
    ...   internal var _base: Base
    ...   @_versioned
    ...   internal let _transform: (Base.Element) -> Element
    ...
    ...   @_inlineable
    ...   @_versioned
    ...   internal init(_base: Base, _transform: @escaping (Base.Element)
    ...     self._base = _base
    ...     self._transform = _transform
    ...   }
    ... }
    ...
    ... /// A `Sequence` whose elements consist of those in a `Base`
    ... /// `Sequence` passed through a transform function returning.
    ... /// These elements are computed lazily, each time they're read, by
    ... /// calling the transform function on a base element.
    ... @_fixed_layout
    ... public struct LazyMapSequence<Base : Sequence, Element>
    ...   : LazySequenceProtocol {
    ...
    ...   public typealias Elements = LazyMapSequence
    ...
    ...   /// Returns an iterator over the elements of this sequence.
    ...   ///
    ...   /// - Complexity: O(1).
    ...   @_inlineable
    ...   public func makeIterator() -> LazyMapIterator<Base.Iterator, Element
    ...     return LazyMapIterator(_base: _base.makeIterator(), _transform: _t
    ...   }
    ...
    ...   /// Returns a value less than or equal to the number of elements in
    ...   /// `self`, **nondestructively**.
    ...   ///
    ...   /// - Complexity: O(*n*)
    ...   @_inlineable
    ...   public var underestimatedCount: Int {
    ...     return _base.underestimatedCount
    ...   }
    ...
    ...   /// Creates an instance with elements `transform(x)` for each elemen
    ...   /// `x` of base.
    ...   @_inlineable
    ...   @_versioned
    ...   internal init(_base: Base, transform: @escaping (Base.Iterator.Eleme
    ...     self._base = _base
    ...     self._transform = transform
    ...   }
    ...
    ...   @_versioned
    ...   internal var _base: Base
    ...   @_versioned
    ...   internal let _transform: (Base.Iterator.Element) -> Element
    ... }
    ...
    ... //===--- Collections -------------------------------------------------
    ...
    ... // FIXME(ABI)#45 (Conditional Conformance): `LazyMap*Collection` types
    ... // collapsed into one `LazyMapCollection` using conditional conformanc
    ... // Maybe even combined with `LazyMapSequence`.
    ... // rdar://problem/17144340
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 108)
    ...
    ... /// A `Collection` whose elements consist of those in a `Base`
    ... /// `Collection` passed through a transform function returning `Elemen
    ... /// These elements are computed lazily, each time they're read, by
    ... /// calling the transform function on a base element.
    ... @_fixed_layout
    ... public struct LazyMapCollection<
    ...   Base : Collection, Element
    ... > : LazyCollectionProtocol,
    ...     _CollectionWrapper
    ... {
    ...   public typealias Base = Base_
    ...   public typealias Index = Base.Index
    ...   public typealias _Element = Base._Element
    ...   public typealias SubSequence = Base.SubSequence
    ...   typealias Indices = Base.Indices
    ...
    ...   @_inlineable
    ...   public subscript(position: Base.Index) -> Element {
    ...     return _transform(_base[position])
    ...   }
    ...
    ...   /// Create an instance with elements `transform(x)` for each element
    ...   /// `x` of base.
    ...   @_inlineable
    ...   @_versioned
    ...   internal init(_base: Base, transform: @escaping (Base.Iterator.Eleme
    ...     self._base = _base
    ...     self._transform = transform
    ...   }
    ...
    ...   @_versioned
    ...   internal var _base: Base
    ...   @_versioned
    ...   internal let _transform: (Base.Iterator.Element) -> Element
    ... }
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 108)
    ...
    ... /// A `Collection` whose elements consist of those in a `Base`
    ... /// `Collection` passed through a transform function returning `Elemen
    ... /// These elements are computed lazily, each time they're read, by
    ... /// calling the transform function on a base element.
    ... @_fixed_layout
    ... public struct LazyMapBidirectionalCollection<
    ...   Base : BidirectionalCollection, Element
    ... > : LazyCollectionProtocol,
    ...     _BidirectionalCollectionWrapper
    ... {
    ...   public typealias Base = Base_
    ...   public typealias Index = Base.Index
    ...   public typealias _Element = Base._Element
    ...   public typealias SubSequence = Base.SubSequence
    ...   typealias Indices = Base.Indices
    ...
    ...   @_inlineable
    ...   public subscript(position: Base.Index) -> Element {
    ...     return _transform(_base[position])
    ...   }
    ...
    ...   /// Create an instance with elements `transform(x)` for each element
    ...   /// `x` of base.
    ...   @_inlineable
    ...   @_versioned
    ...   internal init(_base: Base, transform: @escaping (Base.Iterator.Eleme
    ...     self._base = _base
    ...     self._transform = transform
    ...   }
    ...
    ...   @_versioned
    ...   internal var _base: Base
    ...   @_versioned
    ...   internal let _transform: (Base.Iterator.Element) -> Element
    ... }
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 108)
    ...
    ... /// A `Collection` whose elements consist of those in a `Base`
    ... /// `Collection` passed through a transform function returning `Elemen
    ... /// These elements are computed lazily, each time they're read, by
    ... /// calling the transform function on a base element.
    ... @_fixed_layout
    ... public struct LazyMapRandomAccessCollection<
    ...   Base : RandomAccessCollection, Element
    ... > : LazyCollectionProtocol,
    ...     _RandomAccessCollectionWrapper
    ... {
    ...   public typealias Base = Base_
    ...   public typealias Index = Base.Index
    ...   public typealias _Element = Base._Element
    ...   public typealias SubSequence = Base.SubSequence
    ...   typealias Indices = Base.Indices
    ...
    ...   @_inlineable
    ...   public subscript(position: Base.Index) -> Element {
    ...     return _transform(_base[position])
    ...   }
    ...
    ...   /// Create an instance with elements `transform(x)` for each element
    ...   /// `x` of base.
    ...   @_inlineable
    ...   @_versioned
    ...   internal init(_base: Base, transform: @escaping (Base.Iterator.Eleme
    ...     self._base = _base
    ...     self._transform = transform
    ...   }
    ...
    ...   @_versioned
    ...   internal var _base: Base
    ...   @_versioned
    ...   internal let _transform: (Base.Iterator.Element) -> Element
    ... }
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 146)
    ...
    ... //===--- Support for s.lazy ------------------------------------------
    ...
    ... extension LazySequenceProtocol {
    ...   /// Returns a `LazyMapSequence` over this `Sequence`.  The elements
    ...   /// the result are computed lazily, each time they are read, by
    ...   /// calling `transform` function on a base element.
    ...   @_inlineable
    ...   public func map<U>(
    ...     _ transform: @escaping (Elements.Iterator.Element) -> U
    ...   ) -> LazyMapSequence<Self.Elements, U> {
    ...     return LazyMapSequence(_base: self.elements, transform: transform)
    ...   }
    ... }
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 162)
    ...
    ... extension LazyCollectionProtocol
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 169)
    ... {
    ...   /// Returns a `LazyMapCollection` over this `Collection`.  The eleme
    ...   /// the result are computed lazily, each time they are read, by
    ...   /// calling `transform` function on a base element.
    ...   @_inlineable
    ...   public func map<U>(
    ...     _ transform: @escaping (Elements.Iterator.Element) -> U
    ...   ) -> LazyMapCollection<Self.Elements, U> {
    ...     return LazyMapCollection(
    ...       _base: self.elements,
    ...       transform: transform)
    ...   }
    ... }
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 162)
    ...
    ... extension LazyCollectionProtocol
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 165)
    ...   where
    ...   Self : BidirectionalCollection,
    ...   Elements : BidirectionalCollection
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 169)
    ... {
    ...   /// Returns a `LazyMapCollection` over this `Collection`.  The eleme
    ...   /// the result are computed lazily, each time they are read, by
    ...   /// calling `transform` function on a base element.
    ...   @_inlineable
    ...   public func map<U>(
    ...     _ transform: @escaping (Elements.Iterator.Element) -> U
    ...   ) -> LazyMapBidirectionalCollection<Self.Elements, U> {
    ...     return LazyMapBidirectionalCollection(
    ...       _base: self.elements,
    ...       transform: transform)
    ...   }
    ... }
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 162)
    ...
    ... extension LazyCollectionProtocol
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 165)
    ...   where
    ...   Self : RandomAccessCollection,
    ...   Elements : RandomAccessCollection
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 169)
    ... {
    ...   /// Returns a `LazyMapCollection` over this `Collection`.  The eleme
    ...   /// the result are computed lazily, each time they are read, by
    ...   /// calling `transform` function on a base element.
    ...   @_inlineable
    ...   public func map<U>(
    ...     _ transform: @escaping (Elements.Iterator.Element) -> U
    ...   ) -> LazyMapRandomAccessCollection<Self.Elements, U> {
    ...     return LazyMapRandomAccessCollection(
    ...       _base: self.elements,
    ...       transform: transform)
    ...   }
    ... }
    ...
    ... // ###sourceLocation(file: "/public/core/Map.swift.gyb", line: 184)
    ...
    ... @available(*, unavailable, renamed: "LazyMapIterator")
    ... public struct LazyMapGenerator<Base : IteratorProtocol, Element> {}
    ...
    ... extension LazyMapSequence {
    ...   @available(*, unavailable, message: "use '.lazy.map' on the sequence
    ...   public init(_ base: Base, transform: (Base.Iterator.Element) -> Elem
    ...     Builtin.unreachable()
    ...   }
    ... }
    ...
    ... extension LazyMapCollection {
    ...   @available(*, unavailable, message: "use '.lazy.map' on the collecti
    ...   public init(_ base: Base, transform: (Base.Iterator.Element) -> Elem
    ...     Builtin.unreachable()
    ...   }
    ... }
    ...
    ... // Local Variables:
    ... // eval: (read-only-mode 1)
    ... // End:
    ... ''')
    >>> long_output.flush()
    >>> long_output_result = subprocess.check_output(sys.executable + ' ' +
    ...    __file__ + ' ' + long_output.name + ' -- ' + "echo '" +
    ...    long_output.name + ":112:27: error:'",
    ...    shell=True).rstrip()
    >>> print(long_output_result)
    /public/core/Map.swift.gyb:117:27: error:
    >>> target1.close()
    >>> os.remove(target1.name)
    >>> target2.close()
    >>> os.remove(target2.name)
    >>> raw_output.close()
    >>> os.remove(raw_output.name)

    Lint this file.
    >>> import python_lint
    >>> python_lint.lint([os.path.realpath(__file__)], verbose=False)
    0
    """
    if len(sys.argv) <= 1:
        import doctest
        failure_count, _ = doctest.testmod()
        sys.exit(failure_count)
    elif '--' not in sys.argv:
        source_file = sys.argv[1]
        source_line = int(sys.argv[2])
        target_file = sys.argv[3]
        print(map_line_from_source_file(source_file, source_line, target_file))
    else:
        dashes = sys.argv.index('--')
        sources = expand_response_files(sys.argv[1:dashes])

        # The first argument of command_args is the process to open.
        # subprocess.Popen doesn't normalize arguments. This means that trying
        # to open a non-normalized file (e.g. C:/swift/./bin/swiftc.exe) on
        # Windows results in file/directory not found errors, as Popen
        # delegates to the Win32 CreateProcess API. Unix systems handle
        # non-normalized paths, so don't have this problem.
        # Arguments passed to the process are normalized by the process.
        command_args = expand_response_files(sys.argv[dashes + 1:])
        command_args[0] = os.path.normpath(command_args[0])

        command = subprocess.Popen(
            command_args,
            stderr=subprocess.STDOUT,
            stdout=subprocess.PIPE,
            universal_newlines=True
        )

        sources = '(?P<file>' + '|'.join(re.escape(s) for s in sources) + ')'

        error_pattern = re.compile(
            '^' + sources +
            ':(?P<line>[0-9]+):(?P<column>[0-9]+):(?P<tail>.*?)\n?$')

        assertion_pattern = re.compile(
            '^(?P<head>.*( file | at |#[0-9]+: |[[]))' +
            sources +
            '(?P<middle>, line |:)(?P<line>[0-9]+)(?P<tail>.*?)\n?$')

        while True:
            input = command.stdout.readline()
            if input == '':
                break
            output = input

            def decode_match(p, l):
                m = p.match(l)
                if m is None:
                    return ()
                file, line_num = map_line_to_source_file(
                    m.group('file'), int(m.group('line')))
                return ((m, file, line_num),)

            for (m, file, line_num) in decode_match(error_pattern, input):
                output = '%s:%s:%s:%s\n' % (
                    file, line_num, int(m.group(3)), m.group(4))
                break
            else:
                for (m, file, line_num) in decode_match(assertion_pattern,
                                                        input):
                    output = '%s%s%s%s%s\n' % (
                        m.group('head'), file, m.group('middle'), line_num,
                        m.group('tail'))
            sys.stdout.write(output)

        sys.exit(command.wait())


if __name__ == '__main__':
    run()
