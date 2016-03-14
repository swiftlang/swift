// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A{{}{}enum B<T where T:d}struct Q{struct B{enum A{class A{struct S<T where B:A
