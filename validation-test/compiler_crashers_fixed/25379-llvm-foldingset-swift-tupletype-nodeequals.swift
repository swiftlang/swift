// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func g{struct B<d{enum S{struct a:A{}class A{}}}struct B<T where g:a{class B
