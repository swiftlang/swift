// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class b{func a{b=b}var f{class A{class b{class A{struct X<T:T.f
