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
    class func b() {
        struct c {
            static let d: String = {
                return self  }
}
func b(c) -> <d>(() -> d) {
}
struct d<f(b: c) {
        self.b = b
   }
}
var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
}(f, e)
import Foundation
extension NSSet {
    convenience init<T>(array: Array<T>) {
        self.init()
    }
}
