// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -parse -verify

// Issue found by https://github.com/jvasileff (John Vasileff)
// This bug is NOT triggered when compiling with -O.

protocol BooleanProtocol {
  var boolValue: Bool { get }
}
extension Bool : BooleanProtocol {
  var boolValue: Bool { return self }
}
func f<T : BooleanProtocol>(_ b: T) {
}
f(true as BooleanProtocol) // expected-error {{cannot invoke 'f' with an argument list of type '(BooleanProtocol)'}} // expected-note {{expected an argument list of type '(T)'}}
