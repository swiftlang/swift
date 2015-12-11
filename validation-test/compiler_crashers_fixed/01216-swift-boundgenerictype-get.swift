// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class B<C> {
let c: C
init(c: C) {
self.c = c
class A {
class func a() {
n self.a
