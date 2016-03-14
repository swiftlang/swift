// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let c{b<:{var a{{{{var b{protocol a{struct S{{}struct A{class B{struct S{class A{func a{class A{var:{let a{{enum a{let a{{enum S{class A{enum b{var b{{b=a
