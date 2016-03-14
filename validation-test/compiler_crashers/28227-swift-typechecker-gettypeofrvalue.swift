// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
struct c{struct A:a{var d}let a=A{}protocol A
