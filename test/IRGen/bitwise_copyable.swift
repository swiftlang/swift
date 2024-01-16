// RUN: %target-swift-frontend                           \
// RUN:     -primary-file %s                             \
// RUN:     -emit-ir                                     \
// RUN:     -disable-availability-checking               \
// RUN:     -enable-experimental-feature BitwiseCopyable \
// RUN:     -enable-builtin-module

// REQUIRES: asserts

// Force verification of TypeLowering's isTrivial.

import Builtin

struct Box<T : _BitwiseCopyable> : _BitwiseCopyable {
  var t: T
}

struct Boxx<each T : _BitwiseCopyable> {
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
