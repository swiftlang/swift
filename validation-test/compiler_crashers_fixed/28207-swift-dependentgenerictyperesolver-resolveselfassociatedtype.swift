// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
// REQUIRES: asserts
protocol A{typealias e
func f:A
protocol A{typealias A:f func f:e
