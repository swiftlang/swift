// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var d="
{class b{struct B<T where g:a{
class A{struct A<I:e class b<a
let:{b{
