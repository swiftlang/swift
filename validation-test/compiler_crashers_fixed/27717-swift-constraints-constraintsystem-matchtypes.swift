// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T{let a{
class A
class b:A
struct B<T where f:T{
class A
class A{
let a{A?
