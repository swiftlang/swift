// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func c<A:A.a{{{a{}{}{let a{func a{{{let a{var b{d{struct B{class b{let a{{}struct B{class a{{}class a{var b
