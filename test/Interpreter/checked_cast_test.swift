//===--- checked_cast_test.swift ------------------------------------------===//
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

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Test that we emit proper ARC here and do not crash.

private class Foo {
  init() {}
}

final private class Bar : Foo {
}

final private class Baz : Foo {
}

private func checked_cast_default(_ f: Foo) -> Int {
  switch f {
  case is Bar:
    return 0
  case is Baz:
    return 1
  default:
    return 2
  }
}

private func checked_cast_exhaustive(_ f: Foo) -> Int {
  switch f {
  case is Bar:
    return 0
  case is Baz:
    return 1
  case is Foo:
    return 2
  }
}

func main() {
  let b = Baz()
  let x = checked_cast_default(b)
  precondition(x == 1)
  let y = checked_cast_exhaustive(b)
  precondition(y == 1)
}

main()
