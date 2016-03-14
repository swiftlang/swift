// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// DUPLICATE-OF: 26832-swift-typechecker-conformstoprotocol.swift
// RUN: not --crash %target-swift-frontend %s -parse
class B<T{enum S<h{protocol A{{
}
func g:A
struct A:B<T>
struct B<a
