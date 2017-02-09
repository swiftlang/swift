//===--- TypeFlood.swift --------------------------------------------------===//
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
//
// We use this test to benchmark the runtime memory that Swift programs use.
//
// The Swift compiler caches the metadata that it needs to generate to support
// code that checks for protocol conformance and other operations that require
// use of metadata.
// This mechanism has the potential to allocate a lot of memory. This benchmark
// program generates 2^15 calls to swift_conformsToProtocol and fills the
// metadata/conformance caches with data that we never free. We use this
// program to track the runtime memory usage of swift programs. The test is
// optimized away in Release builds but kept in Debug mode.


import TestsUtils
protocol Pingable {}

struct Some1<T> {
  init() {}
  func foo(_ x: T) {}
}
struct Some0<T> {
  init() {}
  func foo(_ x: T) {}
}

@inline(never)
func flood<T>(_ x: T) {
  _ = Some1<Some1<Some1<Some1<T>>>>() is Pingable
  _ = Some1<Some1<Some1<Some0<T>>>>() is Pingable
  _ = Some1<Some1<Some0<Some1<T>>>>() is Pingable
  _ = Some1<Some1<Some0<Some0<T>>>>() is Pingable
  _ = Some1<Some0<Some1<Some1<T>>>>() is Pingable
  _ = Some1<Some0<Some1<Some0<T>>>>() is Pingable
  _ = Some1<Some0<Some0<Some1<T>>>>() is Pingable
  _ = Some1<Some0<Some0<Some0<T>>>>() is Pingable
  _ = Some0<Some1<Some1<Some1<T>>>>() is Pingable
  _ = Some0<Some1<Some1<Some0<T>>>>() is Pingable
  _ = Some0<Some1<Some0<Some1<T>>>>() is Pingable
  _ = Some0<Some1<Some0<Some0<T>>>>() is Pingable
  _ = Some0<Some0<Some1<Some1<T>>>>() is Pingable
  _ = Some0<Some0<Some1<Some0<T>>>>() is Pingable
  _ = Some0<Some0<Some0<Some1<T>>>>() is Pingable
  _ = Some0<Some0<Some0<Some0<T>>>>() is Pingable
}

@inline(never)
func flood3<T>(_ x: T) {
 flood(Some1<Some1<Some1<Some1<T>>>>())
 flood(Some1<Some1<Some1<Some0<T>>>>())
 flood(Some1<Some1<Some0<Some1<T>>>>())
 flood(Some1<Some1<Some0<Some0<T>>>>())
 flood(Some1<Some0<Some1<Some1<T>>>>())
 flood(Some1<Some0<Some1<Some0<T>>>>())
 flood(Some1<Some0<Some0<Some1<T>>>>())
 flood(Some1<Some0<Some0<Some0<T>>>>())
 flood(Some0<Some1<Some1<Some1<T>>>>())
 flood(Some0<Some1<Some1<Some0<T>>>>())
 flood(Some0<Some1<Some0<Some1<T>>>>())
 flood(Some0<Some1<Some0<Some0<T>>>>())
 flood(Some0<Some0<Some1<Some1<T>>>>())
 flood(Some0<Some0<Some1<Some0<T>>>>())
 flood(Some0<Some0<Some0<Some1<T>>>>())
 flood(Some0<Some0<Some0<Some0<T>>>>())
}

@inline(never)
func flood2<T>(_ x: T) {
 flood3(Some1<Some1<Some1<Some1<T>>>>())
 flood3(Some1<Some1<Some1<Some0<T>>>>())
 flood3(Some1<Some1<Some0<Some1<T>>>>())
 flood3(Some1<Some1<Some0<Some0<T>>>>())
 flood3(Some1<Some0<Some1<Some1<T>>>>())
 flood3(Some1<Some0<Some1<Some0<T>>>>())
 flood3(Some1<Some0<Some0<Some1<T>>>>())
 flood3(Some1<Some0<Some0<Some0<T>>>>())
 flood3(Some0<Some1<Some1<Some1<T>>>>())
 flood3(Some0<Some1<Some1<Some0<T>>>>())
 flood3(Some0<Some1<Some0<Some1<T>>>>())
 flood3(Some0<Some1<Some0<Some0<T>>>>())
 flood3(Some0<Some0<Some1<Some1<T>>>>())
 flood3(Some0<Some0<Some1<Some0<T>>>>())
 flood3(Some0<Some0<Some0<Some1<T>>>>())
 flood3(Some0<Some0<Some0<Some0<T>>>>())
}

@inline(never)
public func run_TypeFlood(_ N: Int) {

  for _ in 1...N {
    flood3(Some1<Some1<Some1<Int>>>())
    flood3(Some1<Some1<Some0<Int>>>())
    flood3(Some1<Some0<Some1<Int>>>())
    flood3(Some1<Some0<Some0<Int>>>())
    flood3(Some0<Some1<Some1<Int>>>())
    flood3(Some0<Some1<Some0<Int>>>())
    flood3(Some0<Some0<Some1<Int>>>())
    flood3(Some0<Some0<Some0<Int>>>())
  }
}
