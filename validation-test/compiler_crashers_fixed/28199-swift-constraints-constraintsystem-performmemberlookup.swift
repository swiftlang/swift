// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
// REQUIRES: asserts
class A{
struct S<>:A var e=A.e
class A:e{typealias e=s
