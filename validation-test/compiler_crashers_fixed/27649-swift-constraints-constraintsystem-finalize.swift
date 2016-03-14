// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<T where g:T{struct D{class b<g:func d{{}{}{}enum a{class d{var f=b
