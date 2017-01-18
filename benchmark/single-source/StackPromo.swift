//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

protocol Proto {
  func at() -> Int
}

@inline(never)
func testStackAllocation(_ p: Proto) -> Int {
  var a = [p, p, p]
  var b = 0
  a.withUnsafeMutableBufferPointer {
    let array = $0
    for i in 0..<array.count {
      b += array[i].at()
    }
  }
  return b
}

class Foo : Proto {
  init() {}
  func at() -> Int{
    return 1
  }
}

@inline(never)
func work(_ f: Foo) -> Int {
  var r = 0
  for _ in 0..<100_000 {
    r += testStackAllocation(f)
  }
  return r
}

@inline(never)
func hole(_ use: Int, _ N: Int) {
  if (N == 0) {
    print("use: \(use)")
  }
}

public func run_StackPromo(_ N: Int) {
  let foo = Foo()
  var r = 0
  for i in 0..<N {
    if i % 2 == 0 {
      r += work(foo)
    } else {
      r -= work(foo)
    }
  }
  hole(r, N)
}
