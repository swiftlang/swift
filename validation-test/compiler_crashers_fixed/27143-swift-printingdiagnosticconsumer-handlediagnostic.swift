// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{func o}struct A{class B<T,A{class a{struct S<D{class d{struct A{class a<T{class B:a
