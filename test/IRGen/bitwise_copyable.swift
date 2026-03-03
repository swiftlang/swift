// RUN: %target-swift-frontend                           \
// RUN:     -primary-file %s                             \
// RUN:     -emit-ir                                     \
// RUN:     -target %target-swift-5.9-abi-triple         \
// RUN:     -enable-builtin-module

// REQUIRES: asserts
// REQUIRES: objc_interop

// Force verification of TypeLowering's isTrivial.

import Foundation
import Builtin

struct Box<T : BitwiseCopyable> : BitwiseCopyable {
  var t: T
}

struct Boxx<each T : BitwiseCopyable> {
  var ts: (repeat each T)
}

func nameBuiltinInteger(_ b: Builtin.Int64) {}
func nameBuiltinFloat(_ b: Builtin.FPIEEE64) {}
func nameBuiltinPackIndex(_ b: Builtin.PackIndex) {}
func nameBuiltinRawPointer(_ b: Builtin.RawPointer) {}
func nameBuiltinVector(_ b: Builtin.Vec2xInt65) {}
func nameBuiltinExecutor(_ b: Builtin.Executor) {}
func nameBuiltinJob(_ b: Builtin.Job) {}
func nameBuiltinRawUnsafeContinuation(_ b: Builtin.RawUnsafeContinuation) {}
func nameBuiltinNativeObject(_ b: Builtin.NativeObject) {}
func nameBuiltinBridgeObject(_ b: Builtin.BridgeObject) {}
func nameBuiltinUnsafeValueBuffer(_ b: Builtin.UnsafeValueBuffer) {}
func nameBuiltinDefaultActorStorage(_ b: Builtin.DefaultActorStorage) {}
func nameBuiltinNonDefaultDistributedActorStorage(_ b: Builtin.NonDefaultDistributedActorStorage) {}

struct MyObjCBool {
  var value: ObjCBool
}
