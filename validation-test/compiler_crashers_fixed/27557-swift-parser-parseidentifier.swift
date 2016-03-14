// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<T where a:a{class a{struct d{class B:a{protocol C{class b{class B<where T{}}
