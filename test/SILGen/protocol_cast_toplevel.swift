// RUN: %target-swift-emit-silgen %s -enable-objc-interop -disable-objc-attr-requires-foundation-module

@objc protocol Unrelated {}

@objc class C {}

let c = C()
let unrelated = c as! Unrelated
