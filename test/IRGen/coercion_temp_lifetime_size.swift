// RUN: %target-swift-frontend -parse-stdlib -enable-builtin-module -emit-ir %s | %FileCheck %s

// The temporary alloca created for NativeConventionSchema coercion
// (irgen::allocateForCoercion) must emit its llvm.lifetime markers with a size
// equal to the temporary's size in *bytes*, not in *bits*.
//
// The size was computed with DataLayout::getTypeSizeInBits() and then handed to
// a byte-denominated `Size`, producing an 8x-oversized lifetime range (e.g. 64
// for an 8-byte temporary). That is harmless in isolation, but once such a
// coercion temporary is coalesced into an async coroutine frame by CoroSplit,
// the oversized lifetime.end spans adjacent, still-live frame slots (such as a
// spilled `self`). DSE then legally drops those live stores, miscompiling the
// function (self reads back as zero). See rdar/GenCall.cpp allocateForCoercion.

// REQUIRES: PTRSIZE=64

import Builtin

struct B { var v: Builtin.Int8 }

// An 8-byte value type whose ABI lowering coerces through memory to `{ i64 }`.
enum E { case p((B, B, B, B, B, B, B, B)) }

// CHECK-LABEL: define {{.*}} @"$s{{.*}}4take{{.*}}"
// The coercion temporary is an 8-byte alloca...
// CHECK: %temp-coercion.coerced = alloca { i64 }
// ...so its lifetime marker must cover 8 bytes, not 64 (the size in bits).
// CHECK-NOT: @llvm.lifetime.start.p0(i64 64, ptr %temp-coercion.coerced)
// CHECK: call void @llvm.lifetime.start.p0(i64 8, ptr %temp-coercion.coerced)
func take(_ e: E) -> B {
  switch e { case .p(let t): return t.0 }
}
