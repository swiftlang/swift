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
// RUN: %if swift_stdlib_no_asserts %{ %target-swift-frontend %s -module-name SpanConversions -parse-as-library -target %target-swift-6.2-abi-triple -enable-experimental-feature Lifetimes -emit-ir -O -o %t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-VIEWASINT8 --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-UNSAFEBYTESASBYTE --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-BYTESOFBOOL --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-UNSAFEBYTESOFBYTE --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-MUTATINGVIEWASINT8 --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-CONSUMINGVIEWASINT8 --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-MUTABLEBYTESOFINT8 --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-CONVERTTORAWINT8 --input-file=%t.ll %}
// RUN: %if swift_stdlib_no_asserts %{ %FileCheck %s --check-prefix=IR-UNSAFEMUTABLEBYTESOFBYTE --input-file=%t.ll %}

// REQUIRES: swift_feature_Lifetimes

// Conversions between raw and typed spans for 1-byte elements should involve
// no calculations.

// The IR of each function is checked in its own FileCheck run. The module also
// holds code that isn't under test (such as the value witnesses that Windows
// emits for `Byte`), and function merging makes the order of the functions
// unpredictable.

// SIL-LABEL: sil {{.*}}@$s{{.*}}viewAsInt8{{.*}} :
// SIL:         bb0({{.*}} : $RawSpan):
// SIL-NEXT:      debug_value
// SIL-NEXT:      struct_extract {{.*}}, #RawSpan._pointer
// SIL-NEXT:      struct_extract {{.*}}, #RawSpan._count
// SIL-NEXT:      struct $Span<Int8>
// SIL-NEXT:      mark_dependence [nonescaping]
// SIL-NEXT:      return
// SIL-NEXT:    }
// IR-VIEWASINT8-LABEL: define {{.*}}@"$s{{.*}}viewAsInt8{{.*}}"(
// IR-VIEWASINT8-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-VIEWASINT8:       ret
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
// IR-UNSAFEBYTESASBYTE-LABEL: define {{.*}}@"$s{{.*}}unsafeBytesAsByte{{.*}}"(
// IR-UNSAFEBYTESASBYTE-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-UNSAFEBYTESASBYTE:       ret
@_lifetime(copy r)
public func unsafeBytesAsByte(_ r: RawSpan) -> Span<Byte> {
  unsafe Span(_unsafeBytes: r)
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}bytesOfBool{{.*}} :
// IR-BYTESOFBOOL-LABEL: define {{.*}}@"$s{{.*}}bytesOfBool{{.*}}"(
// IR-BYTESOFBOOL-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-BYTESOFBOOL:       ret
@_lifetime(copy s)
public func bytesOfBool(_ s: Span<Bool>) -> RawSpan {
  s.bytes
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}unsafeBytesOfByte{{.*}} :
// IR-UNSAFEBYTESOFBYTE-LABEL: define {{.*}}@"$s{{.*}}unsafeBytesOfByte{{.*}}"(
// IR-UNSAFEBYTESOFBYTE-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-UNSAFEBYTESOFBYTE:       ret
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
// IR-MUTATINGVIEWASINT8-LABEL: define {{.*}}@"$s{{.*}}mutatingViewAsInt8{{.*}}"(
// IR-MUTATINGVIEWASINT8-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-MUTATINGVIEWASINT8:       ret
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
// IR-CONSUMINGVIEWASINT8-LABEL: define {{.*}}@"$s{{.*}}consumingViewAsInt8{{.*}}"(
// IR-CONSUMINGVIEWASINT8-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-CONSUMINGVIEWASINT8:       ret
@_lifetime(copy r)
public func consumingViewAsInt8(
  _ r: consuming MutableRawSpan
) -> MutableSpan<Int8> {
  MutableSpan(mutableBytes: r)
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}mutableBytesOfInt8{{.*}} :
// IR-MUTABLEBYTESOFINT8-LABEL: define {{.*}}@"$s{{.*}}mutableBytesOfInt8{{.*}}"(
// IR-MUTABLEBYTESOFINT8-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-MUTABLEBYTESOFINT8:       ret
@_lifetime(&s)
public func mutableBytesOfInt8(
  _ s: inout MutableSpan<Int8>
) -> MutableRawSpan {
  s.mutableBytes
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}convertToRawInt8{{.*}} :
// IR-CONVERTTORAWINT8-LABEL: define {{.*}}@"$s{{.*}}convertToRawInt8{{.*}}"(
// IR-CONVERTTORAWINT8-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-CONVERTTORAWINT8:       ret
@_lifetime(copy s)
public func convertToRawInt8(
  _ s: consuming MutableSpan<Int8>
) -> MutableRawSpan {
  MutableRawSpan(elements: s)
}

// SIL-LABEL: sil {{.*}}@$s{{.*}}unsafeMutableBytesOfByte{{.*}} :
// IR-UNSAFEMUTABLEBYTESOFBYTE-LABEL: define {{.*}}@"$s{{.*}}unsafeMutableBytesOfByte{{.*}}"(
// IR-UNSAFEMUTABLEBYTESOFBYTE-NOT:   {{icmp (eq|ne|slt|sle|sge|u[a-z]+)|select|br (i1|label)|alloca|swift_allocObject|= (add|sub|mul|shl|lshr|ashr|and|or|xor)[ ]}}
// IR-UNSAFEMUTABLEBYTESOFBYTE:       ret
@_lifetime(&s)
public func unsafeMutableBytesOfByte(
  _ s: inout MutableSpan<Byte>
) -> MutableRawSpan {
  unsafe s.mutableBytes
}
