// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<C> {
let c: C
init(c: C) {
self.c = c
class A {
class func a() {
n self.a
