// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S{struct Q<T where g:P{class B{let d=A}}struct A{class d{struct A{struct Q{enum e:Boolean
