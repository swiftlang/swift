// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B{var _=0.b}struct B<T where g:B{struct B{{}func c{let h=c{
