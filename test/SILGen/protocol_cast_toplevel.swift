// RUN: %swift -emit-silgen %s

@objc protocol Unrelated {}

@objc class C {}

let c = C()
let unrelated = c as Unrelated
