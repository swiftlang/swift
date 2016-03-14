// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let b{c{{}}{struct Q<T where g:a{class A{class B:A{struct B<T where g:o
