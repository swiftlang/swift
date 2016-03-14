// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var:{[{class a<T where a<I>:a{class A{class a<T{{}class B:A{var b
