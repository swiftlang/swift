// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
enum S<T where g:a{enum A{class c{let t}struct a{let a=c()}}struct S{var _=A
