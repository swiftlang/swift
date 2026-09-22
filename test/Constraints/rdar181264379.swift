// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop
import Foundation
import CoreFoundation

let n: NSNumber = 1
let d = [
    "a": n,
    "b": kCFBooleanTrue,
]
_ = d

func f(_: [String: NSNumber?].Type) {}

let t = type(of: d)
f(t)
