// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<T {
struct c<T {
var f = d<S
struct S<T
func d
struct S<T where S<T> : A {
struct c<d where T = c
