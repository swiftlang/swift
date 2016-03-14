// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{enum a{enum S{struct A{struct B<T where B:a{class A{let t=(
