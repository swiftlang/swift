// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func b<T{{struct S{struct d{protocol a}}struct S<d where T.B:b
