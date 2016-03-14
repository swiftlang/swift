// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func g{enum b{struct S{enum B<d{func c{enum S<T:T.B
