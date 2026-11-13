// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify -swift-version 5 %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify -swift-version 5 -O %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify -swift-version 6 %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify -swift-version 6 -O %s

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// https://github.com/swiftlang/swift/issues/93630
// A method reference forms a curry thunk whose borrowed self parameter cannot
// be captured by the escaping inner closure. Diagnose this instead of boxing
// the borrowed value and emitting a copy_addr with an object operand.
// The self parameter of the implicit thunk has no source location.
// expected-error@<unknown> 4 {{'self' cannot be captured by an escaping closure since it is a borrowed parameter}}

struct NC: ~Copyable {
  func method() {}
  static func staticMethod() {}
}

enum NCEnum: ~Copyable {
  case value
  func method() {}
}

_ = NC.method // expected-note {{closure capturing 'self' here}}
_ = NCEnum.method // expected-note {{closure capturing 'self' here}}

func bound(_ value: consuming NC) {
  _ = value.method // expected-note {{closure capturing 'self' here}}
}

func applied() {
  NC.method(NC())() // expected-note {{closure capturing 'self' here}}
}

// Calling a method directly and referencing a static method do not need to
// capture a borrowed noncopyable value.
func direct(_ value: borrowing NC) {
  value.method()
}
_ = NC.staticMethod

// A copyable receiver can still be captured by a method-reference thunk.
struct CopyableValue {
  func method() {}
}
CopyableValue.method(CopyableValue())()
