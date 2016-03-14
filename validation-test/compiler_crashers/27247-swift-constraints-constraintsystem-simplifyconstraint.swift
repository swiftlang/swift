// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// DUPLICATE-OF: 26832-swift-typechecker-conformstoprotocol.swift
// RUN: not --crash %target-swift-frontend %s -parse
class C struct B<B{struct B<T:C{protocol d{let:B<T>{}struct B<b
