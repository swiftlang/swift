// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{
class A c{class A:A:class B<T{{{{{}}}}class b:A
}}class B<T where h:C{class b
let f=b(
