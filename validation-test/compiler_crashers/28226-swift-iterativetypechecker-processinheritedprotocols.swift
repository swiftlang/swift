// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
protocol A{associatedtype e:A protocol A:a
protocol a:c}struct c<I:A
