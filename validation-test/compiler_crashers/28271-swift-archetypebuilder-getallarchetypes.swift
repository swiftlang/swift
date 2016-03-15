// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
func b{{
protocol P{func b
func b<T where T=e
typealias e
