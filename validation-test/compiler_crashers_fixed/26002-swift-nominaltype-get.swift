// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func g{class B<T{class B{class A<T where g:A{enum k:A<T>
