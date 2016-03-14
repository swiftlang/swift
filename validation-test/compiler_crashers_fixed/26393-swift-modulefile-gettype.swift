// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A{protocol A:Collection
protocol A:A protocol A{typealias h:b
func b<I
