// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T where B:P{{}
struct d{{{
}}
class B<b
let a=B
