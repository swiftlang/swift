#===--- protocol_graph.py -------------------------*- coding: utf-8 -*----===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#
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
#===----------------------------------------------------------------------===#

import re
import sys
import os
import cgi

# Open 'stdlib.swift' in this directory if no path specified.
args = list(sys.argv) + [os.path.join(os.path.dirname(__file__), 'stdlib.swift')]

reFlags = re.MULTILINE | re.VERBOSE

# Pattern to recognize stdlib identifiers (FIXME: doesn't handle Unicode).
identifier = '[A-Za-z_][A-Za-z0-9_]*'

# Pattern to recognize a (possibly-generic) operator decl.
operator = r'''
(?:(?:prefix|postfix).*)? func \s*
(?=\S)[^A-Za-z_]                # non-space, non-identifier: begins an operator name
(?:(?=\S)[^(){])*               # rest of operator name
\s*
(<[^>{]+>)?                     # generic parameter list
\s*
\([^)]*\)                       # function parameter list
'''

# substitute local variables into the string
def interpolate(string):
    import inspect
    frame = inspect.currentframe()
    return string % frame.f_back.f_locals

# Given the bodyText of a protocol definition, return a list of
# associated type and operator requirements.
def bodyLines(bodyText):
    return [
        cgi.escape(b.group(0)) for b in
        re.finditer(
            r'(typealias\s*'+identifier+r'(\s*[:,]\s*'+identifier + ')|' + operator + '.*)',
            bodyText, reFlags)
    ]

# Mapping from protocol to associated type / operator requirements
body = {}

# Mapping from a parent protocol to set of children.
graph = {}

# Mapping from protocol to generic operators taking instances as arguments
genericOperators = {}

comments = r'//.* | /[*] (.|\n)*? [*]/'  # FIXME: doesn't respect strings or comment nesting)

# read source, stripping all comments
sourceSansComments = re.sub(comments, '', open(args[1]).read(), flags=reFlags)

genericParameterConstraint = interpolate(r' (%(identifier)s) \s* : \s* (%(identifier)s) ')

def parseGenericOperator(m):
    genericParams = m.group(5)
    genericOperator = cgi.escape(m.group(0).strip())
    functionParamStart = m.end(5) - m.start(0)
    functionParams = genericOperator[functionParamStart:]
    for m2 in re.finditer(genericParameterConstraint, genericParams, reFlags):
        typeParameter = m2.group(1)
        protocol = m2.group(2)
        
        # we're only interested if we can find a function parameter of that type
        if not re.search(r':\s*%s\s*[,)]' % typeParameter, functionParams): continue

        # Make some replacements in the signature to limit the graph size
        letterTau = '&#x3c4;'
        letterPi = '&#x3c0;'
        abbreviatedSignature = re.sub(
            r'\b%s\b' % protocol, letterPi,
            re.sub(r'\b%s\b' % typeParameter, letterTau, genericOperator))

        genericOperators.setdefault(protocol, set()).add(abbreviatedSignature)

def parseProtocol(m):
    child = m.group(1)
    # skip irrelevant protocols
    if re.match(r'_Builtin.*Convertible', child): return 
    graph.setdefault(child, set())
    body[child] = bodyLines(m.group(3))
    if m.group(2):
        for parent in m.group(2).strip().split(","):
            if re.match(r'_Builtin.*Convertible', parent): return
            graph.setdefault(parent.strip(), set()).add(child)

protocolsAndOperators = interpolate(r'''
\bprotocol \s+ (%(identifier)s) \s*
  (?::\s*([^{]+))?                   # refinements
  {([^{}\n]*(.*\n)*?)}               # body
|
%(operator)s [^{]*(?={)              # operator definition up to the open brace
''')

# Main parsing loop
for m in re.finditer(protocolsAndOperators, sourceSansComments, reFlags):
    if m.group(1): parseProtocol(m)
    elif m.group(5): parseGenericOperator(m)
    # otherwise we matched some non-generic operator

# Find clusters of protocols that have the same name when underscores
# are stripped
clusterBuilder = {} # map from potential cluster name to nodes in the cluster
for n in graph:
    clusterBuilder.setdefault(n.translate(None, '_'), set()).add(n)

# Grab the clusters with more than one member.
clusters = dict((c, nodes) for (c, nodes) in clusterBuilder.items() if len(nodes) > 1)

# A set of all intra-cluster edges
clusterEdges = set(
    (s, t) for (c, elements) in clusters.items()
    for s in elements 
    for t in graph[s] if t in elements)

print 'digraph ProtocolHierarchies {'
print '  mclimit = 100; ranksep=1.5; ' # ; packmode="array1"
print '  edge [dir="back"];'
print '  node [shape = box, fontname = Helvetica, fontsize = 10];'

for c in sorted(clusters):
    print '  subgraph "cluster_%s" {' % c
    for (s, t) in sorted(clusterEdges):
        if s in clusters[c]:
            print '%s -> %s [weight=100];' % (s, t)
    print '}'

for node in sorted(graph.keys()):
    requirements = body.get(node, [])
    generics = sorted(genericOperators.get(node, set()))
    style = 'solid' if node.startswith('_') else 'bold'
    divider = '<HR/>\n' if len(requirements) != 0 and len(generics) != 0 else ''
    
    label = node if len(requirements + generics) == 0 else (
        '\n<TABLE BORDER="0">\n<TR><TD>\n%s\n</TD></TR><HR/>\n%s%s%s</TABLE>\n' % (
        node,
        '\n'.join('<TR><TD>%s</TD></TR>' % r for r in requirements),
        divider,
        '\n'.join('<TR><TD>%s</TD></TR>' % g for g in generics)))
    
    print interpolate('    %(node)s [style = %(style)s, label=<%(label)s>]')
    
for (parent, children) in sorted(graph.items()):
    print '    %s -> {' % parent,
    print '; '.join(
        sorted(child for child in children if not (parent, child) in clusterEdges)),
    print '}'

print '}'    
