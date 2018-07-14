//===--- OpaqueConsumingUsers.swift ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let OpaqueConsumingUsers = BenchmarkInfo(
  name: "OpaqueConsumingUsers",
  runFunction: run_OpaqueConsumingUsers,
  tags: [.regression, .abstraction, .refcount],
  setUpFunction: setup_OpaqueConsumingUsers)

// This test exercises the ability of the optimizer to propagate the +1 from a
// consuming argument of a non-inlineable through multiple non-inlinable call
// frames.
//
// We are trying to simulate a user application that calls into a resilient
// setter or initialization. We want to be able to propagate the +1 from the
// setter through the app as far as we can.

class Klass {}

class ConsumingUser {
  var _innerValue: Klass = Klass()

  var value: Klass {
    @inline(never) get {
      return _innerValue
    }
    @inline(never) set {
      _innerValue = newValue
    }
  }
}

var data: Klass? = nil
var user: ConsumingUser? = nil

func setup_OpaqueConsumingUsers() {
  switch (data, user) {
  case (let x?, let y?):
    let _ = x
    let _ = y
    return
  case (nil, nil):
    data = Klass()
    user = ConsumingUser()
  default:
    fatalError("Data and user should both be .none or .some")
  }
}

@inline(never)
func callFrame1(_ data: Klass, _ user: ConsumingUser) {
  callFrame2(data, user)
}

@inline(never)
func callFrame2(_ data: Klass, _ user: ConsumingUser) {
  callFrame3(data, user)
}

@inline(never)
func callFrame3(_ data: Klass, _ user: ConsumingUser) {
  callFrame4(data, user)
}

@inline(never)
func callFrame4(_ data: Klass, _ user: ConsumingUser) {
  user.value = data
}

@inline(never)
public func run_OpaqueConsumingUsers(_ N: Int) {
  let d = data._unsafelyUnwrappedUnchecked
  let u = user._unsafelyUnwrappedUnchecked
  for _ in 0..<N*200000 {
    callFrame4(d, u)
  }
}
