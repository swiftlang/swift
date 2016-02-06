// DUPLICATE-OF: 26832-swift-typechecker-conformstoprotocol.swift
// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct B<T{
class B<I:B{protocol C{struct B<T
var:B<T>
