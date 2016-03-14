// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
protocol c{associatedtype e:a:protocol a{associatedtype a}func b:e.c
