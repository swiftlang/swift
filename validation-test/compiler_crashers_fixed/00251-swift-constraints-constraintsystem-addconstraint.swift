// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
import Foundation
class A {
    class func a() -> String {
        return ""
    }
  struct c {
            svar e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
}(f, e)
func a(b: Int = 0) {
}
let c = a
c()
func b(c) -> <d>(() -> d) {
}
enu: P {
   func f<T>() -> T -> T {
        return { x in x }
    }
}
protocol P {
    func f<T>()(T) -> T
}
protocol A {
    type 
