// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<T where g=b{
class c{let a=b}let a=0.g
struct A{var _=c{
