// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func<{struct B{{}enum e:a}class a<T where I:A{struct Q{{}class A<T{let a=A{
