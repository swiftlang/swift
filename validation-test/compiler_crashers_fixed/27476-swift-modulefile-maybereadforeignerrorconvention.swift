// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if{struct T{enum S<T where g:a{class A{class a{let f:Collection
