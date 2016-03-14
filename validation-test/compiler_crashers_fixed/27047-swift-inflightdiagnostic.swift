// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let a{{struct D{{}enum A{let a{var b{let a{{var b{struct D{var b{(a=a
