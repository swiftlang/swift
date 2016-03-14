// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func b>class B<T{let t:T
struct f:Array
let A{{b{{
