//===--- ArrayLiteral.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test checks performance of creating array from literal and array value
// propagation.
// It is reported to be slow: <rdar://problem/17297449>
import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "ArrayLiteral2", runFunction: run_ArrayLiteral, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayValueProp", runFunction: run_ArrayValueProp, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayValueProp2", runFunction: run_ArrayValueProp2, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayValueProp3", runFunction: run_ArrayValueProp3, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayValueProp4", runFunction: run_ArrayValueProp4, tags: [.validation, .api, .Array]),
]

@inline(never)
func makeArray() -> [Int] {
  return [1,2,3]
}

@inline(never)
public func run_ArrayLiteral(_ n: Int) {
  for _ in 1...10000*n {
    blackHole(makeArray())
  }
}

@inline(never)
func addLiteralArray() -> Int {
  let arr = [1, 2, 3]
  return arr[0] + arr[1] + arr[2]
}

@inline(never)
public func run_ArrayValueProp(_ n: Int) {
  var res = 123
  for _ in 1...10000*n {
    res += addLiteralArray()
    res -= addLiteralArray()
  }
  check(res == 123)
}


@inline(never)
func addLiteralArray2() -> Int {
  let arr = [1, 2, 3]
  var r = 0
  for elt in arr {
    r += elt
  }
  return r
}

@inline(never)
func addLiteralArray3() -> Int {
  let arr = [1, 2, 3]
  var r = 0
  for i in 0..<arr.count {
    r += arr[i]
  }
  return r
}

@inline(never)
func addLiteralArray4() -> Int {
  let arr = [1, 2, 3]
  var r = 0
  for i in 0..<3 {
    r += arr[i]
  }
  return r
}

@inline(never)
public func run_ArrayValueProp2(_ n: Int) {
  var res = 123
  for _ in 1...10000*n {
    res += addLiteralArray2()
    res -= addLiteralArray2()
  }
  check(res == 123)
}

@inline(never)
public func run_ArrayValueProp3(_ n: Int) {
  var res = 123
  for _ in 1...10000*n {
    res += addLiteralArray3()
    res -= addLiteralArray3()
  }
  check(res == 123)
}

@inline(never)
public func run_ArrayValueProp4(_ n: Int) {
  var res = 123
  for _ in 1...10000*n {
    res += addLiteralArray4()
    res -= addLiteralArray4()
  }
  check(res == 123)
}
