// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{class a<H,A{class d<a{enum S{class a{struct B}}class B<T where T:c{class c:d
