# ===--- protocol_graph.py ---------------------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//
#
# Create a graph of the protocol refinement relationships, associated
# types, operator requirements, and defaulted generic operators.
#
# run as follows to view the Nth-largest connected component in a web browser:
#
#   N=0 && rm -f /tmp/protocols.dot && \
#   python protocol_graph.py stdlib.swift > /tmp/p0.dot && \
#   (ccomps -zX#$N -o /tmp/protocols.dot /tmp/p0.dot || true) \
#   && dot -Tsvg /tmp/protocols.dot > /tmp/protocols.svg \
#   && open /tmp/protocols.svg
#
# ===---------------------------------------------------------------------===//

from __future__ import print_function

import cgi
import os
import re
import sys

# Open 'stdlib.swift' in this directory if no path specified.
args = list(sys.argv) + \
    [os.path.join(os.path.dirname(__file__), 'stdlib.swift')]

re_flags = re.MULTILINE | re.VERBOSE

# Pattern to recognize stdlib identifiers (FIXME: doesn't handle Unicode).
identifier = '[A-Za-z_][A-Za-z0-9_]*'

# Pattern to recognize a (possibly-generic) operator decl.
operator = r'''
(?:(?:prefix|postfix).*)? func \s*
(?=\S)[^A-Za-z_]           # non-space, non-identifier: begins an operator name
(?:(?=\S)[^(){])*          # rest of operator name
\s*
(<[^>{]+>)?                # generic parameter list
\s*
\([^)]*\)                  # function parameter list
'''

# substitute local variables into the string


def interpolate(string):
    import inspect
    frame = inspect.currentframe()
    return string % frame.f_back.f_locals

# Given the body_text of a protocol definition, return a list of
# associated type and operator requirements.


def body_lines(body_text):
    return [
        cgi.escape(b.group(0)) for b in
        re.finditer(
            r'(typealias\s*' + identifier +
            r'(\s*[:,]\s*' + identifier + ')?|' + operator + '.*)',
            body_text, re_flags)
    ]


# Mapping from protocol to associated type / operator requirements
body = {}

# Mapping from a parent protocol to set of children.
graph = {}

# Mapping from protocol to generic operators taking instances as arguments
generic_operators = {}

# FIXME: doesn't respect strings or comment nesting)
comments = r'//.* | /[*] (.|\n)*? [*]/'

# read source, stripping all comments
with open(args[1]) as src:
    source_sans_comments = re.sub(comments, '', src.read(), flags=re_flags)

generic_parameter_constraint = interpolate(
    r' (%(identifier)s) \s* : \s* (%(identifier)s) ')


def parse_generic_operator(m):
    generic_params = m.group(5)
    generic_operator = cgi.escape(m.group(0).strip())
    function_param_start = m.end(5) - m.start(0)
    function_params = generic_operator[function_param_start:]
    for m2 in re.finditer(
            generic_parameter_constraint, generic_params, re_flags):
        type_parameter = m2.group(1)
        protocol = m2.group(2)

        # we're only interested if we can find a function parameter of that
        # type
        if not re.search(r':\s*%s\s*[,)]' % type_parameter, function_params):
            continue

        # Make some replacements in the signature to limit the graph size
        letter_tau = '&#x3c4;'
        letter_pi = '&#x3c0;'
        abbreviated_signature = re.sub(
            r'\b%s\b' % protocol, letter_pi,
            re.sub(r'\b%s\b' % type_parameter, letter_tau, generic_operator))

        generic_operators.setdefault(
            protocol, set()).add(abbreviated_signature)


def parse_protocol(m):
    child = m.group(1)
    # skip irrelevant protocols
    if re.match(r'_Builtin.*Convertible', child):
        return
    graph.setdefault(child, set())
    body[child] = body_lines(m.group(3))
    if m.group(2):
        for parent in m.group(2).strip().split(","):
            if re.match(r'_Builtin.*Convertible', parent):
                return
            graph.setdefault(parent.strip(), set()).add(child)


protocols_and_operators = interpolate(r'''
\bprotocol \s+ (%(identifier)s) \s*
  (?::\s*([^{]+))?                   # refinements
  {([^{}\n]*(.*\n)*?)}               # body
|
%(operator)s [^{]*(?={)              # operator definition up to the open brace
''')

# Main parsing loop
for m in re.finditer(protocols_and_operators, source_sans_comments, re_flags):
    if m.group(1):
        parse_protocol(m)
    elif m.group(5):
        parse_generic_operator(m)
    # otherwise we matched some non-generic operator

# Find clusters of protocols that have the same name when underscores
# are stripped
# map from potential cluster name to nodes in the cluster
cluster_builder = {}
for n in graph:
    cluster_builder.setdefault(n.translate(None, '_'), set()).add(n)

# Grab the clusters with more than one member.
clusters = dict((c, nodes)
                for (c, nodes) in cluster_builder.items() if len(nodes) > 1)

# A set of all intra-cluster edges
cluster_edges = set(
    (s, t) for (c, elements) in clusters.items()
    for s in elements
    for t in graph[s] if t in elements)

print('digraph ProtocolHierarchies {')
# ; packmode="array1"
print('  mclimit = 100; ranksep=1.5; ')
print('  edge [dir="back"];')
print('  node [shape = box, fontname = Helvetica, fontsize = 10];')

for c in sorted(clusters):
    print('  subgraph "cluster_%s" {' % c)
    for (s, t) in sorted(cluster_edges):
        if s in clusters[c]:
            print('%s -> %s [weight=100];' % (s, t))
    print('}')

for node in sorted(graph.keys()):
    requirements = body.get(node, [])
    generics = sorted(generic_operators.get(node, set()))
    style = 'solid' if node.startswith('_') else 'bold'
    divider = '<HR/>\n' if len(requirements) != 0 and len(generics) != 0 \
              else ''

    label = node if len(requirements + generics) == 0 else (
        '\n<TABLE BORDER="0">\n<TR><TD>\n%s\n</TD></TR><HR/>' +
        '\n%s%s%s</TABLE>\n' % (
            node,
            '\n'.join('<TR><TD>%s</TD></TR>' % r for r in requirements),
            divider,
            '\n'.join('<TR><TD>%s</TD></TR>' % g for g in generics)))

    print(interpolate('    %(node)s [style = %(style)s, label=<%(label)s>]'))
for (parent, children) in sorted(graph.items()):
    print('    %s -> {' % parent, end=' ')
    print('; '.join(sorted(
        child for child in children
        if not (parent, child) in cluster_edges)
    ), end=' ')
    print('}')

print('}')
