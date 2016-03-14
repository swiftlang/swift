// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B{struct S<T where g:T{
var _=Void
class b{
func a{{{
b{
