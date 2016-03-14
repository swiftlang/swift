// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{enum B:a=class b{struct S<B{struct B<b{enum B{enum B{enum A{class b<f:f.c
