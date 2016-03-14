// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

if{enum S<T{enum S:d<T>}typealias d<T where B:T>:a
