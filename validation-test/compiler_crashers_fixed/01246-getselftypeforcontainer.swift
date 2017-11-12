// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func p(l: Any, g: Any) -> (((Any, Any) -> Any) -> Any) {
return {
(p: (Any, Any) -> Any) -> Any in
func n<n : l,) {
}
var e: Int -> Int = {
}
let d: Int =  { c, b in
}(f, e)
struct c<d : Sequence> {
var b:  [c<d>] {
return []
}
protocol a {
}
class b: a {
}
func f<T : Boolean>(b: T) {
}
func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> A  var d: b.Type
protocol c : b { func b
