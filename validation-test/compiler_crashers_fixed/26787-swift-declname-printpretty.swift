// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a{struct B<T where g:a{struct S<h{class A{struct B<struct B<T where g:a{var:A
