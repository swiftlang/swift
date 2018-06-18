// RUN: %target-swift-emit-silgen %s -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-sil-ownership

@objc protocol Unrelated {}

@objc class C {}

let c = C()
let unrelated = c as! Unrelated
