//===--- TypeFlood.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
protocol Pingable {}

struct Some1<T> {
  init() {}
  func foo(x: T) {}
}
struct Some0<T> {
  init() {}
  func foo(x: T) {}
}

@inline(never)
func flood<T>(x : T) {
  Some1<Some1<Some1<Some1<T>>>>() is Pingable
  Some1<Some1<Some1<Some0<T>>>>() is Pingable
  Some1<Some1<Some0<Some1<T>>>>() is Pingable
  Some1<Some1<Some0<Some0<T>>>>() is Pingable
  Some1<Some0<Some1<Some1<T>>>>() is Pingable
  Some1<Some0<Some1<Some0<T>>>>() is Pingable
  Some1<Some0<Some0<Some1<T>>>>() is Pingable
  Some1<Some0<Some0<Some0<T>>>>() is Pingable
  Some0<Some1<Some1<Some1<T>>>>() is Pingable
  Some0<Some1<Some1<Some0<T>>>>() is Pingable
  Some0<Some1<Some0<Some1<T>>>>() is Pingable
  Some0<Some1<Some0<Some0<T>>>>() is Pingable
  Some0<Some0<Some1<Some1<T>>>>() is Pingable
  Some0<Some0<Some1<Some0<T>>>>() is Pingable
  Some0<Some0<Some0<Some1<T>>>>() is Pingable
  Some0<Some0<Some0<Some0<T>>>>() is Pingable
}

@inline(never)
func flood3<T>(x : T) {
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
func flood2<T>(x : T) {
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
public func run_TypeFlood(N: Int) {

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
