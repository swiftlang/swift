// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class S<T{func a<h{func b<T where h.g=a{}}struct b{{}{}}struct b{struct S{enum a{{}class A{struct g{struct Q{{}}enum b
