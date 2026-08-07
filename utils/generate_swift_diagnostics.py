#!/usr/bin/env python3
# utils/generate_swift_diagnostics.py - Generate Swift bindings for diagnostics
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

"""Generates Swift bindings for the diagnostics declared in a
``Diagnostics*.def`` file, so that Swift code in the compiler (notably ASTGen)
can emit the compiler's existing diagnostics rather than ad-hoc strings.

For each diagnostic, a static member is emitted on ``CompilerDiagnostic`` whose
name is the diagnostic's ID and whose parameters mirror the diagnostic's
signature.  A diagnostic with no arguments becomes a static property; one with
arguments becomes a static function::

    /// error: expected expression
    static var expected_expr: Self { ... }

    /// error: expected ')' in '%0' attribute
    static func attr_expected_rparen(_ a0: String, _ a1: Bool) -> Self { ... }

The generated code refers to each diagnostic *symbolically*, as
``swift.DiagID.<id>``, rather than by numeric value.  ``DiagID`` is imported
into Swift from ``include/swift/AST/DiagnosticList.h``, so the Swift compiler
itself checks that every generated name corresponds to a real diagnostic: a
misparse here is a build failure, not a silently mismatched diagnostic.

Diagnostics whose signatures mention argument types that cannot yet be
constructed from Swift are skipped, and listed in a comment at the end of the
generated file.
"""

import argparse
import os
import re
import sys

# C++ diagnostic argument type -> (Swift parameter type, CompilerDiagnosticArgument case)
#
# Only types that Swift code can meaningfully produce while building an AST are
# supported.  Everything else (Type, const Decl *, DeclName, ...) names a
# semantic entity that is not available at ASTGen time; diagnostics using them
# are skipped rather than exposed with an untypeable parameter.
ARGUMENT_TYPES = {
    'StringRef': ('String', 'string'),
    'Identifier': ('Identifier', 'identifier'),
    'bool': ('Bool', 'boolean'),
    'unsigned': ('UInt', 'unsigned'),
    'int': ('Int', 'integer'),
}

DIAGNOSTIC_MACROS = {
    # macro name -> field names, in order.  The signature is always last.
    'ERROR': ('id', 'options', 'text', 'signature'),
    'WARNING': ('id', 'options', 'text', 'signature'),
    'NOTE': ('id', 'options', 'text', 'signature'),
    'REMARK': ('id', 'options', 'text', 'signature'),
    'GROUPED_ERROR': ('id', 'group', 'options', 'text', 'signature'),
    'GROUPED_WARNING': ('id', 'group', 'options', 'text', 'signature'),
    'GROUPED_NOTE': ('id', 'group', 'options', 'text', 'signature'),
}

KIND_FOR_MACRO = {
    'ERROR': 'error', 'GROUPED_ERROR': 'error',
    'WARNING': 'warning', 'GROUPED_WARNING': 'warning',
    'NOTE': 'note', 'GROUPED_NOTE': 'note',
    'REMARK': 'remark',
}

MACRO_START = re.compile(
    r'(?<![A-Za-z_0-9])(' + '|'.join(sorted(DIAGNOSTIC_MACROS, key=len, reverse=True)) +
    r')\s*\(')


def strip_comments(text):
    """Blank out // and /* */ comments, preserving offsets and newlines."""
    out = []
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c == '"' or c == "'":
            quote = c
            out.append(c)
            i += 1
            while i < n:
                if text[i] == '\\':
                    out.append(text[i:i + 2])
                    i += 2
                    continue
                out.append(text[i])
                if text[i] == quote:
                    i += 1
                    break
                i += 1
            continue
        if c == '/' and i + 1 < n and text[i + 1] == '/':
            while i < n and text[i] != '\n':
                out.append(' ')
                i += 1
            continue
        if c == '/' and i + 1 < n and text[i + 1] == '*':
            end = text.find('*/', i + 2)
            end = n if end < 0 else end + 2
            out.append(''.join('\n' if ch == '\n' else ' ' for ch in text[i:end]))
            i = end
            continue
        out.append(c)
        i += 1
    return ''.join(out)


def split_arguments(text, start):
    """Split the macro argument list beginning at ``text[start] == '('``.

    Returns ``(fields, end)`` where ``fields`` are the top-level,
    comma-separated arguments and ``end`` is the index just past the closing
    paren.  Respects nested parens/brackets/angles and string and character
    literals -- diagnostic text routinely contains things like "expected ')'",
    which naive splitting gets wrong.
    """
    assert text[start] == '('
    fields = []
    depth = 0
    field_start = start + 1
    i, n = start, len(text)
    while i < n:
        c = text[i]
        if c in '"\'':
            quote = c
            i += 1
            while i < n:
                if text[i] == '\\':
                    i += 2
                    continue
                if text[i] == quote:
                    break
                i += 1
            i += 1
            continue
        if c in '([{<':
            depth += 1
        elif c in ')]}>':
            depth -= 1
            if depth == 0:
                fields.append(text[field_start:i])
                return fields, i + 1
        elif c == ',' and depth == 1:
            fields.append(text[field_start:i])
            field_start = i + 1
        i += 1
    raise ValueError('unterminated macro argument list at offset {}'.format(start))


def parse_text_literal(field):
    """Join the adjacent C string literals making up a diagnostic's text."""
    pieces = re.findall(r'"((?:[^"\\]|\\.)*)"', field)
    text = ''.join(pieces)
    return (text.replace('\\"', '"').replace('\\n', ' ').replace('\\t', ' ')
                .replace('\\\\', '\\'))


def parse_signature(field):
    """Parse a diagnostic signature such as ``(StringRef, bool)`` into a list
    of C++ type names.  Returns None if the field is not a signature."""
    field = field.strip()
    if not (field.startswith('(') and field.endswith(')')):
        return None
    inner = field[1:-1].strip()
    if not inner:
        return []
    types, depth, current = [], 0, ''
    for c in inner:
        if c in '(<[':
            depth += 1
        elif c in ')>]':
            depth -= 1
        if c == ',' and depth == 0:
            types.append(current.strip())
            current = ''
            continue
        current += c
    types.append(current.strip())
    return types


def parse_def_file(path):
    """Yield one record per diagnostic declared in ``path``."""
    text = strip_comments(open(path, encoding='utf-8').read())
    results = []
    pos = 0
    while True:
        m = MACRO_START.search(text, pos)
        if not m:
            break
        macro = m.group(1)
        paren = m.end() - 1
        try:
            fields, end = split_arguments(text, paren)
        except ValueError as e:
            raise ValueError('{}: {}'.format(path, e))
        pos = end
        names = DIAGNOSTIC_MACROS[macro]
        if len(fields) != len(names):
            # Not a diagnostic declaration (e.g. the #define in
            # DefineDiagnosticMacros.h being echoed); skip it.
            continue
        record = dict(zip(names, fields))
        identifier = record['id'].strip()
        if not re.fullmatch(r'[A-Za-z_][A-Za-z_0-9]*', identifier):
            continue
        signature = parse_signature(record['signature'])
        if signature is None:
            continue
        results.append({
            'id': identifier,
            'kind': KIND_FOR_MACRO[macro],
            'text': parse_text_literal(record['text']),
            'signature': signature,
        })
    return results


def wrap_doc_comment(kind, text, width=76):
    """Render a diagnostic's text as a Swift doc comment."""
    words = ('{}: {}'.format(kind, ' '.join(text.split()))).split(' ')
    lines, current = [], '///'
    for word in words:
        if len(current) + 1 + len(word) > width and current != '///':
            lines.append(current)
            current = '///  '
        current += ' ' + word
    lines.append(current)
    return lines


def generate(diagnostics, def_name, generator_name):
    out = [
        '//===--- {} - Generated diagnostic bindings ---===//'.format(
            os.path.basename(def_name).replace('.def', '.swift')),
        '//',
        '// This source file is part of the Swift.org open source project',
        '//',
        '// Copyright (c) 2026 Apple Inc. and the Swift project authors',
        '// Licensed under Apache License v2.0 with Runtime Library Exception',
        '//',
        '// See https://swift.org/LICENSE.txt for license information',
        '// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project'
        ' authors',
        '//',
        '//===----------------------------------------------------------------------===//',
        '//',
        '// This file is generated from {} by'.format(def_name),
        '// utils/{}. DO NOT EDIT.'.format(os.path.basename(generator_name)),
        '//',
        '// Member names deliberately match the diagnostic IDs in the .def file,',
        '// rather than Swift naming conventions, so that a diagnostic can be',
        '// grepped for identically in C++ (`diag::expected_expr`) and Swift',
        '// (`.expected_expr`).',
        '//',
        '//===----------------------------------------------------------------------===//',
        '',
        'extension CompilerDiagnostic {',
    ]

    skipped = []
    emitted = 0
    for diag in diagnostics:
        unsupported = [t for t in diag['signature'] if t not in ARGUMENT_TYPES]
        if unsupported:
            skipped.append((diag['id'], diag['signature']))
            continue

        out.append('')
        out.extend('  ' + line for line in wrap_doc_comment(diag['kind'], diag['text']))
        if not diag['signature']:
            out.append('  static var {}: Self {{'.format(diag['id']))
            out.append('    Self(.{})'.format(diag['id']))
            out.append('  }')
        else:
            params, arguments = [], []
            for index, cxx_type in enumerate(diag['signature']):
                swift_type, case = ARGUMENT_TYPES[cxx_type]
                params.append('_ a{}: {}'.format(index, swift_type))
                arguments.append('.{}(a{})'.format(case, index))
            out.append('  static func {}({}) -> Self {{'.format(
                diag['id'], ', '.join(params)))
            out.append('    Self(.{}, [{}])'.format(diag['id'], ', '.join(arguments)))
            out.append('  }')
        emitted += 1

    out.append('}')

    if skipped:
        out.append('')
        out.append('// The following {} diagnostics are not yet available from Swift,'
                   .format(len(skipped)))
        out.append('// because their signatures mention argument types that Swift code')
        out.append('// cannot currently construct. To expose one, teach')
        out.append('// CompilerDiagnosticArgument (and ARGUMENT_TYPES in the generator)')
        out.append('// how to bridge the missing type.')
        out.append('//')
        for identifier, signature in skipped:
            out.append('//   {}({})'.format(identifier, ', '.join(signature)))

    out.append('')
    return '\n'.join(out), emitted, skipped


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input', help='path to the Diagnostics*.def file')
    parser.add_argument('-o', '--output', required=True,
                        help='path of the Swift file to write')
    parser.add_argument('--verbose', action='store_true',
                        help='report how many diagnostics were emitted/skipped')
    args = parser.parse_args()

    diagnostics = parse_def_file(args.input)
    if not diagnostics:
        sys.exit('error: no diagnostics found in {}'.format(args.input))

    seen = {}
    for diag in diagnostics:
        if diag['id'] in seen:
            sys.exit('error: duplicate diagnostic ID {}'.format(diag['id']))
        seen[diag['id']] = diag

    # Reference the .def path as the compiler sees it, so the generated header
    # comment stays stable regardless of where the build runs.
    def_name = 'include/swift/AST/' + os.path.basename(args.input)
    contents, emitted, skipped = generate(diagnostics, def_name, __file__)

    # Avoid touching the output when nothing changed, so that a reconfigure
    # does not force a rebuild of everything downstream.
    if os.path.exists(args.output):
        with open(args.output, encoding='utf-8') as existing:
            if existing.read() == contents:
                return

    directory = os.path.dirname(args.output)
    if directory:
        os.makedirs(directory, exist_ok=True)
    with open(args.output, 'w', encoding='utf-8') as output:
        output.write(contents)

    if args.verbose:
        print('{}: {} diagnostics, {} available from Swift, {} skipped'.format(
            os.path.basename(args.input), len(diagnostics), emitted, len(skipped)))


if __name__ == '__main__':
    main()
