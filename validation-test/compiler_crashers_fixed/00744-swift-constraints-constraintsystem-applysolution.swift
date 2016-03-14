// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var d = 0
struct Q<T where I.d: A? = b: A {
typealias e = b
