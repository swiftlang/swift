// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a
struct S<T{var d
struct B<T:a let e=B{
