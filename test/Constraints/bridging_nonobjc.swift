// RUN: %target-swift-frontend -typecheck -verify %s -disable-objc-interop


var x: Any = 1
var y = x as AnyObject
