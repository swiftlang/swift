// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
func<{struct c<T{enum S<T{struct A:a protocol a{associatedtype e}struct S<T:A.e
