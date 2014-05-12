// RUN: %swift -emit-silgen %s

@objc @class_protocol protocol Unrelated {}

@objc class C {}

let c = C()
let unrelated = c as Unrelated
