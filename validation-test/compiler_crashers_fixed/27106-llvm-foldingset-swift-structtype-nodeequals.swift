// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var b{enum b{struct A<struct A{class A{var i{}func a{struct d<U:U.a
