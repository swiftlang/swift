// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{let f=[Void{}struct c<T where a:b{class b{class B{}var _=B{
