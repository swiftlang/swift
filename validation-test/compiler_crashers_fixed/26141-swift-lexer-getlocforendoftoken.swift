// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let(){struct B{struct A<T where I:a{class a{var f=B:var f=B
