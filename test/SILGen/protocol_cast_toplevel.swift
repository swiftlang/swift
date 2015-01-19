// RUN: %target-swift-frontend -emit-silgen %s -disable-objc-attr-requires-foundation-module

// REQUIRES: objc_interop

@objc protocol Unrelated {}

@objc class C {}

let c = C()
let unrelated = c as! Unrelated
