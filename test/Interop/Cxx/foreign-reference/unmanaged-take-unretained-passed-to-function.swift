// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-emit-sil -I %t/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -cxx-interoperability-mode=default -disable-availability-checking %t/test.swift | %FileCheck %s

//--- Inputs/module.modulemap
module FRT {
  header "frt.h"
  requires cplusplus
}

//--- Inputs/frt.h
#pragma once

#include <swift/bridging>

struct SWIFT_UNSAFE_REFERENCE Unsafe {
  static Unsafe *_Nonnull create();
};

struct SWIFT_IMMORTAL_REFERENCE Immortal {
  static Immortal *_Nonnull create();
};

struct Shared {
  static Shared *_Nonnull create();
  void retain();
  void release();
} SWIFT_SHARED_REFERENCE(.retain, .release);

inline void takeUnsafe(Unsafe *_Nonnull) {}
inline void takeImmortal(Immortal *_Nonnull) {}
inline void takeShared(Shared *_Nonnull) {}

//--- test.swift
import FRT

// CHECK-LABEL: sil {{.*}}@$s4test9useUnsafeyyF
// CHECK: return
public func useUnsafe() {
  // Used to crash due to ownership mismatch between the trivial
  // unsafe reference type and the expectations of strong copy operations.
  takeUnsafe(Unmanaged.passRetained(Unsafe.create()).takeUnretainedValue())
}

// CHECK-LABEL: sil {{.*}}@$s4test11useImmortalyyF
// CHECK: return
public func useImmortal() {
  takeImmortal(Unmanaged.passRetained(Immortal.create()).takeUnretainedValue())
}

// CHECK-LABEL: sil {{.*}}@$s4test9useSharedyyF
// CHECK: return
public func useShared() {
  takeShared(Unmanaged.passRetained(Shared.create()).takeUnretainedValue())
}
