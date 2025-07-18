//===--- BoundsCheckOptimization.swift ------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-swift-frontend -primary-file %s -O -emit-assembly | %FileCheck %s --check-prefix CHECK --check-prefix CHECK-%target-cpu
// REQUIRES: swift_stdlib_no_asserts

import Swift

func read(index: Int, span: Span<UInt8>) -> UInt8 {
  span[index]
}
// CHECK: s23BoundsCheckOptimization4read5index4spans5UInt8VSi_s4SpanVyAFGtF:

// CHECK-arm64-NOT: tbnz
// CHECK-arm64: cmp
// all unsigned comparison spellings?
// CHECK-arm64-NEXT: b.{{hs|hi|ls|lo|cc|cs}}
// CHECK-arm64-NEXT: ldrb
// CHECK-arm64-NEXT: ret

// CHECK-x86_64-NOT: test
// CHECK-x86_64: cmp
// all unsigned comparison spellings?
// CHECK-x86_64-NEXT: j{{a|ae|b|be|c|na|nae|nb|nbe|nc}}
// CHECK-x86_64-NEXT: movzb
// x86_64 might have a frame pointer operation before ret

func write(value: UInt8, index: Int, span: inout MutableSpan<UInt8>) {
  span[index] = value
}
// CHECK: s23BoundsCheckOptimization5write5value5index4spanys5UInt8V_Sis11MutableSpanVyAGGztF:

// CHECK-arm64-NOT: tbnz
// CHECK-arm64: cmp
// all unsigned comparison spellings?
// CHECK-arm64-NEXT: b.{{hs|hi|ls|lo|cc|cs}}
// no second compare
// CHECK-arm64-NOT: cmp
// no test after compare
// CHECK-arm64-NOT: tbnz
// CHECK-arm64: strb
// CHECK-arm64-NEXT: ret

// CHECK-x86_64-NOT: test
// CHECK-x86_64: cmp
// all unsigned comparison spellings?
// CHECK-x86_64-NEXT: j{{a|ae|b|be|c|na|nae|nb|nbe|nc}}
// no second compare
// CHECK-x86_64-NOT: cmp
// no test after compare
// CHECK-x86_64-NOT: test
// CHECK-x86_64: movb
// x86_64 might have a frame pointer operation before ret
