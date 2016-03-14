// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// DUPLICATE-OF: 24394-swift-typevariabletype-implementation-getrepresentative.swift
// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
struct g
struct c{}protocol c{associatedtype e:d
class d<T where g:e.T
