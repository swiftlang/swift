// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{class a{enum S<T:T.B{}}struct E{let a{{{{}}enum T{class B{let:}}
