// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B{class a<T where H:a{class A{class d{class A:a{}}class a{}}}var _=[]
