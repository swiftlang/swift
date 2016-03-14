// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
class B<T{func a{{func g:A}protocol A{associatedtype e=c<T>struct c<a]associatedtype d:e
