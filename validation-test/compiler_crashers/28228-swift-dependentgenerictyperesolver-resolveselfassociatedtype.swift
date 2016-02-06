// DUPLICATE-OF: 24394-swift-typevariabletype-implementation-getrepresentative.swift
// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct g
struct c{}protocol c{typealias e:d
class d<T where g:e.T
