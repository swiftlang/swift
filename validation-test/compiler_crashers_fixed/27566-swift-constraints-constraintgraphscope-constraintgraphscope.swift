// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let a{var f={func a{func b{class b{protocol C{enum S{class a{func a{{b=1
