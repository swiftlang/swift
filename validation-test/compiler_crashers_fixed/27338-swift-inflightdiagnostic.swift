// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where f=a{{}
struct c
struct a{}{}
struct B<T where g:A
let ed=a{}
let g=c
