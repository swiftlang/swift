// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// DUPLICATE-OF: 26832-swift-typechecker-conformstoprotocol.swift
// RUN: not --crash %target-swift-frontend %s -parse
struct S<T{protocol b{func f:B:class B<c:B<T>
