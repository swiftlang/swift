// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// Crash type: memory error ("Invalid read of size 2")
class A{func b->Self{{{}}class B<n{let a=self
