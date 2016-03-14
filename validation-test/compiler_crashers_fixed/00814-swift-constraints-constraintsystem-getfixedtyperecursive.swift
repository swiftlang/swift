// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func g<T> {
struct S {
var d = 1
let c {
d.c: B<T where g())
c: a {
}
class B : A? {
}
class A
