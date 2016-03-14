// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
[()
typealias e = a
protocol a {
typealias f : Any, f.c
