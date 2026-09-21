// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -import-objc-header %S/Inputs/objc_bridging_nsurl.h %s
// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/objc_bridging_nsurl.h %s

// REQUIRES: objc_interop

// Make sure we do not crash on this

protocol P {
  var outputURL : URL? { get set }
}

extension ObjCKlass : P {}