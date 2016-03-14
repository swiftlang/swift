// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct y{struct a{class B{typealias e=c}}struct c<T where T:b enum a{func b func b<T:T.a
