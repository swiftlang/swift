// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var C{
struct d<T,A{
class B<b{struct d<d{
class A
class B<T>:A
