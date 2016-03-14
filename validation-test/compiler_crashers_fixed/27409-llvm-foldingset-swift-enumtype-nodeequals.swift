// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<a{enum b{}class b<T where k:a{struct S<d{struct c<let a=b<c
