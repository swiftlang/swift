// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/objc_bridging_nsurl.h %s

// REQUIRES: objc_interop

// Make sure we do not crash on this

protocol P {
  var outputURL : URL? { get set }
}

extension ObjCKlass : P {}