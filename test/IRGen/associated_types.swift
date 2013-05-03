// RUN: %swift -emit-llvm -triple=x86_64-apple-darwin10 %s | FileCheck %s
// XFAIL: *
// <rdar://problem/13793646> We don't bind type metadata for associated types
// of generic parameters
protocol Runcer {
  typealias Runcee
}

protocol Runcible {
  typealias RuncerType : Runcer
  typealias AltRuncerType : Runcer
}

struct Owl<T:Runcible, U> {
  func eat(with:T) { }
}
