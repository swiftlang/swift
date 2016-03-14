// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where H:A{""struct Q{struct A{let s=""struct S{let s=A{
