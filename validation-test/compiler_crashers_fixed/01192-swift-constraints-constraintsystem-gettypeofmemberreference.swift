// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
class Foo<T>: NSObject {
init(foo: T) {
(b: Int = 0) {
}
struct c<d : Sequence> {
struct e : d {
}
}
class d: f{  class func i {}
protocol A {
}
class B {
func d() -> String {
}
}
class C: B, A {
override func d() -> String {
}
func c() -> String {
}
}
func e<T where T: A, T: B>(t: T) {
}
func i(c: () -> ()) {
}
var _ = i() {
