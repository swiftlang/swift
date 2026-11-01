//===--- OneByteSpanConversions.swift -------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-swift-frontend %s -module-name SpanConversions -parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature Lifetimes -emit-sil -O | %FileCheck %s --check-prefix=SIL
// RUN: %if swift_stdlib_no_asserts %{ %target-swift-frontend %s -module-name SpanConversions -parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature Lifetimes -emit-ir -O | %FileCheck %s --check-prefix=IR --implicit-check-not='{{icmp (eq|ne|slt|sle|sge|u[a-z]+)}}' --implicit-check-not='select' --implicit-check-not='br i1' --implicit-check-not='br label' --implicit-check-not='alloca' --implicit-check-not='swift_allocObject' --implicit-check-not='{{= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}' %}

// REQUIRES: swift_feature_Lifetimes

// Conversions between raw and typed spans for 1-byte elements should involve
// no calculations.

// IR: target triple
// IR: !llvm.module.flags

// SIL-LABEL: sil {{.*}}@$s{{.*}}viewAsInt8{{.*}} :
// SIL:         bb0({{.*}} : $RawSpan):
// SIL-NEXT:      debug_value
// SIL-NEXT:      struct_extract {{.*}}, #RawSpan._pointer
// SIL-NEXT:      struct_extract {{.*}}, #RawSpan._count
// SIL-NEXT:      struct $Span<Int8>
// SIL-NEXT:      mark_dependence [nonescaping]
// SIL-NEXT:      return
// SIL-NEXT:    }
@_lifetime(copy r)
public func viewAsInt8(_ r: RawSpan) -> Span<Int8> {
  Span(viewing: r)
}

// A 1-byte type not declared as FullyInhabited.
public struct Byte: BitwiseCopyable { public var value: UInt8 }

// SIL-LABEL: sil {{.*}}@$s{{.*}}unsafeBytesAsByte{{.*}} :
// SIL:         bb0({{.*}} : $RawSpan):
// SIL-NEXT:      debug_value
// SIL-NEXT:      struct_extract {{.*}}, #RawSpan._pointer
// SIL-NEXT:      struct_extract {{.*}}, #RawSpan._count
// SIL-NEXT:      struct $Span<Byte>
// SIL-NEXT:      mark_dependence [nonescaping]
// SIL-NEXT:      return
// SIL-NEXT:    }
@_lifetime(copy r)
public func unsafeBytesAsByte(_ r: RawSpan) -> Span<Byte> {
  unsafe Span(_unsafeBytes: r)
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}bytesOfBool{{.*}} :
@_lifetime(copy s)
public func bytesOfBool(_ s: Span<Bool>) -> RawSpan {
  s.bytes
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}unsafeBytesOfByte{{.*}} :
@_lifetime(copy s)
public func unsafeBytesOfByte(_ s: Span<Byte>) -> RawSpan {
  unsafe s.bytes
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}mutatingViewAsInt8{{.*}} :
// SIL:         bb0({{.*}} : $*MutableRawSpan):
// SIL-NEXT:      debug_value
// SIL-NEXT:      struct_element_addr {{.*}}, #MutableRawSpan._pointer
// SIL-NEXT:      load
// SIL-NEXT:      struct_element_addr {{.*}}, #MutableRawSpan._count
// SIL-NEXT:      load
// SIL-NEXT:      struct $MutableSpan<Int8>
// SIL-NEXT:      mark_dependence [nonescaping]
// SIL-NEXT:      mark_dependence [nonescaping]
// SIL-NEXT:      return
// SIL-NEXT:    }
@_lifetime(&r)
public func mutatingViewAsInt8(
  _ r: inout MutableRawSpan
) -> MutableSpan<Int8> {
  MutableSpan(mutating: &r)
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}consumingViewAsInt8{{.*}} :
// SIL:         bb0({{.*}} : $MutableRawSpan):
// SIL-NEXT:      debug_value
// SIL-NEXT:      debug_value undef
// SIL-NEXT:      struct_extract {{.*}}, #MutableRawSpan._count
// SIL-NEXT:      struct_extract {{.*}}, #MutableRawSpan._pointer
// SIL-NEXT:      struct $MutableSpan<Int8>
// SIL-NEXT:      return
// SIL-NEXT:    }
@_lifetime(copy r)
public func consumingViewAsInt8(
  _ r: consuming MutableRawSpan
) -> MutableSpan<Int8> {
  MutableSpan(mutableBytes: r)
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}mutableBytesOfInt8{{.*}} :
@_lifetime(&s)
public func mutableBytesOfInt8(
  _ s: inout MutableSpan<Int8>
) -> MutableRawSpan {
  s.mutableBytes
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}convertToRawInt8{{.*}} :
@_lifetime(copy s)
public func convertToRawInt8(
  _ s: consuming MutableSpan<Int8>
) -> MutableRawSpan {
  MutableRawSpan(elements: s)
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}unsafeMutableBytesOfByte{{.*}} :
@_lifetime(&s)
public func unsafeMutableBytesOfByte(
  _ s: inout MutableSpan<Byte>
) -> MutableRawSpan {
  unsafe s.mutableBytes
}
