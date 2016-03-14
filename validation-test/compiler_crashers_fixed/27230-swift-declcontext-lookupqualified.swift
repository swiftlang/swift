// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T where g:A{
struct B<c{
let a=c<T,protocol A
struct c
class c<T:A
