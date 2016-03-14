// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let a{struct S<a {
struct Q<b{S<b
struct B{
struct b{class b{class A B TA{
}
struct Q<T where h:A{struct {
}struct S<T where h:B:{class B:A{{
b
