// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{(){{}}struct B<T where g:A{class b{class d{{{}}enum S<T where f:a{let a=b{
