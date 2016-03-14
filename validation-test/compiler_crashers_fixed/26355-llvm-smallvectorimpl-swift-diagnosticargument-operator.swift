// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct A}struct S<T where T:A{
struct B{var _=a<r
class a<T>:A
