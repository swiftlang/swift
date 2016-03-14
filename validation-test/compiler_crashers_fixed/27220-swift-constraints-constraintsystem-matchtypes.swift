// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a
class B:a
class A<T where d:T{
class A{func x
func g{x.r=a
