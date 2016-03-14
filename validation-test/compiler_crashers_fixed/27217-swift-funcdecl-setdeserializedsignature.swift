// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{var b{protocol P{let:Boolean}}class B<T where B:a{class B<c{let a=c
