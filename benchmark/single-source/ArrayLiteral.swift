//===--- ArrayLiteral.swift -----------------------------------------------===//
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

// This test checks performance of creating array from literal and array value
// propagation.
// It is reported to be slow: <rdar://problem/17297449>
import TestsUtils

public let ArrayLiteral = [
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
public func run_ArrayLiteral(_ N: Int) {
  for _ in 1...10000*N {
    blackHole(makeArray())
  }
}

@inline(never)
func addLiteralArray() -> Int {
  let Arr = [1, 2, 3]
  return Arr[0] + Arr[1] + Arr[2]
}

@inline(never)
public func run_ArrayValueProp(_ N: Int) {
  var res = 123
  for _ in 1...10000*N {
    res += addLiteralArray()
    res -= addLiteralArray()
  }
  CheckResults(res == 123)
}


@inline(never)
func addLiteralArray2() -> Int {
  let Arr = [1, 2, 3]
  var r = 0
  for elt in Arr {
    r += elt
  }
  return r
}

@inline(never)
func addLiteralArray3() -> Int {
  let Arr = [1, 2, 3]
  var r = 0
  for i in 0..<Arr.count {
    r += Arr[i]
  }
  return r
}

@inline(never)
func addLiteralArray4() -> Int {
  let Arr = [1, 2, 3]
  var r = 0
  for i in 0..<3 {
    r += Arr[i]
  }
  return r
}

@inline(never)
public func run_ArrayValueProp2(_ N: Int) {
  var res = 123
  for _ in 1...10000*N {
    res += addLiteralArray2()
    res -= addLiteralArray2()
  }
  CheckResults(res == 123)
}

@inline(never)
public func run_ArrayValueProp3(_ N: Int) {
  var res = 123
  for _ in 1...10000*N {
    res += addLiteralArray3()
    res -= addLiteralArray3()
  }
  CheckResults(res == 123)
}

@inline(never)
public func run_ArrayValueProp4(_ N: Int) {
  var res = 123
  for _ in 1...10000*N {
    res += addLiteralArray4()
    res -= addLiteralArray4()
  }
  CheckResults(res == 123)
}
