// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{class A{func c{let s=c<I}struct B<T where g:A{class A{var f={
