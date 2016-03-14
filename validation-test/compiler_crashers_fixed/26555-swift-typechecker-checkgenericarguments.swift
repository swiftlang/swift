// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<T where B:C{struct Q<T{class n{struct S{struct A{class a{class d func b{let:d
