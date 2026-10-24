// RUN: %target-swift-frontend %s -emit-ir -O | %FileCheck %s %if CPU=x86_64 %{ --check-prefixes=CHECK,CHECK-X86 %} %else %{ --check-prefixes=CHECK,CHECK-ARM %}

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib
// REQUIRES: CPU=arm64 || CPU=arm64e || CPU=aarch64 || CPU=x86_64

// Lock down which loop shapes LLVM vectorizes at -O, and which it doesn't.

// A failure would be a happy outcome in test cases where no vectorization is currently expected.
// In such a case, simply flip the test to ensure the auto-vectorization is locked in.

@inline(__always)
func hexNibbleInlined(_ n: UInt8) -> UInt8 {
  n > 9 ? n &+ 97 &- 10 : n &+ 48
}

// exK uses uppercase digits so that LLVM doesn't merge it with exJ.
@inline(__always)
func hexNibbleUppercasedInlined(_ n: UInt8) -> UInt8 {
  n > 9 ? n &+ 65 &- 10 : n &+ 48
}

@inline(never)
func hexNibbleNotInlined(_ n: UInt8) -> UInt8 {
  n > 9 ? n &+ 97 &- 10 : n &+ 48
}

// Baseline with checked subscripts: a bounds check stays in the loop: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization12exA_baselineyys4SpanVys5UInt8VG_s07MutableD0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exA_baseline(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  for idx in 0..<16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
  }
}

// Same with unchecked subscripts: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization22exA_baseline_uncheckedyys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exA_baseline_unchecked(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  for idx in 0..<16 {
    let byte = inp[unchecked: idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[unchecked: off] = hexNibbleInlined(high)
    out[unchecked: off &+ 1] = hexNibbleInlined(low)
  }
}

// Same with unsafe pointers: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization26exA_baseline_unsafePointeryySW_SwtF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exA_baseline_unsafePointer(_ inp: UnsafeRawBufferPointer, _ out: UnsafeMutableRawBufferPointer) {
  for idx in 0..<16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
  }
}

// Data-dependent early exit: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization13exB_earlyExityys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exB_earlyExit(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  for idx in 0..<16 {
    let byte = inp[idx]
    if byte == 0xFF { return }
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
  }
}

// Non-inlined call in the body: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization18exC_notInlinedCallyys4SpanVys5UInt8VG_s07MutableF0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exC_notInlinedCall(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  for idx in 0..<16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleNotInlined(high)
    out[off &+ 1] = hexNibbleNotInlined(low)
  }
}

// Data-dependent trip count (search loop): not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization14exD_searchLoopyys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exD_searchLoop(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  var idx = 0
  while inp[idx] != 0 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
    idx &+= 1
  }
}

// Loop-carried dependence: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization14exE_carriedDepyys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exE_carriedDep(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  var acc: UInt8 = 0
  for idx in 0..<16 {
    let byte = inp[idx]
    acc = acc &+ byte
    let off = 2 &* idx
    out[off] = hexNibbleInlined(acc &>> 4)
    out[off &+ 1] = hexNibbleInlined(acc & 0x0F)
  }
}

// Runtime but loop-invariant trip count: a bounds check stays in the loop: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization16exF_runtimeCountyys4SpanVys5UInt8VG_s07MutableE0VyAFGzSitF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exF_runtimeCount(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>, _ n: Int) {
  for idx in 0..<n {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
  }
}

// Same with unchecked subscripts: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization26exF_runtimeCount_uncheckedyys4SpanVys5UInt8VG_s07MutableF0VyAFGzSitF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exF_runtimeCount_unchecked(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>, _ n: Int) {
  for idx in 0..<n {
    let byte = inp[unchecked: idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[unchecked: off] = hexNibbleInlined(high)
    out[unchecked: off &+ 1] = hexNibbleInlined(low)
  }
}

// Same with unsafe pointers: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization30exF_runtimeCount_unsafePointeryySW_SwSitF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exF_runtimeCount_unsafePointer(_ inp: UnsafeRawBufferPointer, _ out: UnsafeMutableRawBufferPointer, _ n: Int) {
  for idx in 0..<n {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
  }
}

// Array subscripts with bounds checks: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization16exG_arrayCheckedyySays5UInt8VG_AEztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exG_arrayChecked(_ inp: [UInt8], _ out: inout [UInt8]) {
  for idx in 0..<16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
  }
}

// Straight-line code with checked subscripts: bounds checks split the basic block: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization7exH_slpyys4SpanVys5UInt8VG_s07MutableD0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exH_slp(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  let b0 = inp[0], b1 = inp[1], b2 = inp[2], b3 = inp[3]
  out[0] = hexNibbleInlined(b0 &>> 4)
  out[1] = hexNibbleInlined(b1 &>> 4)
  out[2] = hexNibbleInlined(b2 &>> 4)
  out[3] = hexNibbleInlined(b3 &>> 4)
}

// Same with unchecked subscripts: SLP-vectorized, not loop-vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization17exH_slp_uncheckedyys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK-NOT:     vector.body
// CHECK:         store <4 x i8>
// CHECK:       {{^}}}
public func exH_slp_unchecked(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  let b0 = inp[unchecked: 0], b1 = inp[unchecked: 1], b2 = inp[unchecked: 2], b3 = inp[unchecked: 3]
  out[unchecked: 0] = hexNibbleInlined(b0 &>> 4)
  out[unchecked: 1] = hexNibbleInlined(b1 &>> 4)
  out[unchecked: 2] = hexNibbleInlined(b2 &>> 4)
  out[unchecked: 3] = hexNibbleInlined(b3 &>> 4)
}

// Same with unsafe pointers: SLP-vectorized, not loop-vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization21exH_slp_unsafePointeryySW_SwtF"(
// CHECK-NOT:     vector.body
// CHECK:         store <4 x i8>
// CHECK:       {{^}}}
public func exH_slp_unsafePointer(_ inp: UnsafeRawBufferPointer, _ out: UnsafeMutableRawBufferPointer) {
  let b0 = inp[0], b1 = inp[1], b2 = inp[2], b3 = inp[3]
  out[0] = hexNibbleInlined(b0 &>> 4)
  out[1] = hexNibbleInlined(b1 &>> 4)
  out[2] = hexNibbleInlined(b2 &>> 4)
  out[3] = hexNibbleInlined(b3 &>> 4)
}

// Checked arithmetic, constant bound: a bounds check stays in the loop: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization16exI_checkedArithyys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exI_checkedArith(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  for idx in 0..<16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 * idx
    out[off] = hexNibbleInlined(high)
    out[off + 1] = hexNibbleInlined(low)
  }
}

// Same with unchecked subscripts: overflow checks fold away: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization26exI_checkedArith_uncheckedyys4SpanVys5UInt8VG_s07MutableF0VyAFGztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exI_checkedArith_unchecked(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  for idx in 0..<16 {
    let byte = inp[unchecked: idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 * idx
    out[unchecked: off] = hexNibbleInlined(high)
    out[unchecked: off + 1] = hexNibbleInlined(low)
  }
}

// Same with unsafe pointers: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization30exI_checkedArith_unsafePointeryySW_SwtF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exI_checkedArith_unsafePointer(_ inp: UnsafeRawBufferPointer, _ out: UnsafeMutableRawBufferPointer) {
  for idx in 0..<16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 * idx
    out[off] = hexNibbleInlined(high)
    out[off + 1] = hexNibbleInlined(low)
  }
}

// Checked arithmetic, runtime bound: overflow check is hoisted: vectorized even with checked subscripts.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization18exL_checkedRuntimeyys4SpanVys5UInt8VG_s07MutableE0VyAFGzSitF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exL_checkedRuntime(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>, _ n: Int) {
  for idx in 0..<n {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 * idx
    out[off] = hexNibbleInlined(high)
    out[off + 1] = hexNibbleInlined(low)
  }
}

// While loop, increment at the end: a bounds check stays in the loop: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization12exJ_whileEndyys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exJ_whileEnd(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  var idx = 0
  while idx < 16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
    idx &+= 1
  }
}

// Same with unchecked subscripts: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization22exJ_whileEnd_uncheckedyys4SpanVys5UInt8VG_s07MutableF0VyAFGztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exJ_whileEnd_unchecked(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  var idx = 0
  while idx < 16 {
    let byte = inp[unchecked: idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[unchecked: off] = hexNibbleInlined(high)
    out[unchecked: off &+ 1] = hexNibbleInlined(low)
    idx &+= 1
  }
}

// Same with unsafe pointers: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization26exJ_whileEnd_unsafePointeryySW_SwtF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exJ_whileEnd_unsafePointer(_ inp: UnsafeRawBufferPointer, _ out: UnsafeMutableRawBufferPointer) {
  var idx = 0
  while idx < 16 {
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleInlined(high)
    out[off &+ 1] = hexNibbleInlined(low)
    idx &+= 1
  }
}

// While loop, increment in defer: a bounds check stays in the loop: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization14exK_whileDeferyys4SpanVys5UInt8VG_s07MutableE0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func exK_whileDefer(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  var idx = 0
  while idx < 16 {
    defer { idx &+= 1 }
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleUppercasedInlined(high)
    out[off &+ 1] = hexNibbleUppercasedInlined(low)
  }
}

// Same with unchecked subscripts: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization24exK_whileDefer_uncheckedyys4SpanVys5UInt8VG_s07MutableF0VyAFGztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exK_whileDefer_unchecked(_ inp: Span<UInt8>, _ out: inout MutableSpan<UInt8>) {
  var idx = 0
  while idx < 16 {
    defer { idx &+= 1 }
    let byte = inp[unchecked: idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[unchecked: off] = hexNibbleUppercasedInlined(high)
    out[unchecked: off &+ 1] = hexNibbleUppercasedInlined(low)
  }
}

// Same with unsafe pointers: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization28exK_whileDefer_unsafePointeryySW_SwtF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func exK_whileDefer_unsafePointer(_ inp: UnsafeRawBufferPointer, _ out: UnsafeMutableRawBufferPointer) {
  var idx = 0
  while idx < 16 {
    defer { idx &+= 1 }
    let byte = inp[idx]
    let high = byte &>> 4
    let low = byte & 0x0F
    let off = 2 &* idx
    out[off] = hexNibbleUppercasedInlined(high)
    out[off &+ 1] = hexNibbleUppercasedInlined(low)
  }
}

// Profitability: operation mix and a tiny trip count.

// Integer divide by a runtime value: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization13m1_divRuntimeyys7RawSpanV_s07MutableeF0Vzs5UInt8VtF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func m1_divRuntime(_ inp: RawSpan, _ out: inout MutableRawSpan, _ d: UInt8) {
  for i in 0..<64 {
    out[i] = inp.load(fromByteOffset: i, as: UInt8.self) / d
  }
}

// Integer divide by a constant: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization11m2_divConstyys7RawSpanV_s07MutableeF0VztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func m2_divConst(_ inp: RawSpan, _ out: inout MutableRawSpan) {
  for i in 0..<64 {
    out[i] = inp.load(fromByteOffset: i, as: UInt8.self) / 3
  }
}

// Tiny constant trip count is fully unrolled: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization11m3_tinyTripyys7RawSpanV_s07MutableeF0VztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func m3_tinyTrip(_ inp: RawSpan, _ out: inout MutableRawSpan) {
  for i in 0..<3 {
    out[i] = inp.load(fromByteOffset: i, as: UInt8.self) &+ 1
  }
}

// Heavy body of vectorizable operations: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization12m4_heavyBodyyys7RawSpanV_s07MutableeF0VztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func m4_heavyBody(_ inp: RawSpan, _ out: inout MutableRawSpan) {
  for i in 0..<64 {
    var x = inp.load(fromByteOffset: i, as: UInt8.self)
    x = x ^ 0x5A
    x = (x &<< 3) | (x &>> 5)
    x = x &+ 0x11
    x = x & 0x7F
    x = x | 0x80
    x = x ^ (x &>> 2)
    x = x &* 7
    x = ~x
    out[i] = x
  }
}

// Popcount: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization11m5_popcountyys7RawSpanV_s07MutabledE0VztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func m5_popcount(_ inp: RawSpan, _ out: inout MutableRawSpan) {
  for i in 0..<64 {
    out[i] = UInt8(inp.load(fromByteOffset: i, as: UInt8.self).nonzeroBitCount)
  }
}

// Remainder by a runtime value: vectorized on ARM, not on x86.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization13m6_remRuntimeyys7RawSpanV_s07MutableeF0Vzs5UInt8VtF"(
// CHECK-ARM:     vector.body:
// CHECK-X86-NOT: {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func m6_remRuntime(_ inp: RawSpan, _ out: inout MutableRawSpan, _ d: UInt8) {
  for i in 0..<64 {
    out[i] = inp.load(fromByteOffset: i, as: UInt8.self) % d
  }
}

// Trip count: full unrolling and the vectorizer's minimum trip count.

// Constant trip count 4 with .load: fully unrolled, not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization2c4yys7RawSpanV_s07MutablecD0VztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func c4(_ i: RawSpan, _ o: inout MutableRawSpan) {
  for k in 0..<4 { o[k] = i.load(fromByteOffset: k, as: UInt8.self) &+ 1 }
}

// Constant trip count 4 with subscript: fully unrolled, not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization6c4_subyys4SpanVys5UInt8VG_s07MutableD0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func c4_sub(_ i: Span<UInt8>, _ o: inout MutableSpan<UInt8>) {
  for k in 0..<4 { o[k] = i[k] &+ 1 }
}

// Trip count 36 is still fully unrolled on ARM: not vectorized. x86 doesn't fully unroll it and vectorizes it.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization3c36yys4SpanVys5UInt8VG_s07MutableC0VyAFGztF"(
// CHECK-ARM-NOT: {{<[0-9]+ x }}
// CHECK-X86:     vector.body:
// CHECK:       {{^}}}
public func c36(_ i: Span<UInt8>, _ o: inout MutableSpan<UInt8>) {
  for k in 0..<36 { o[k] = i[k] &+ 1 }
}

// Trip count 37 is past the full-unroll limit: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization3c37yys4SpanVys5UInt8VG_s07MutableC0VyAFGztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func c37(_ i: Span<UInt8>, _ o: inout MutableSpan<UInt8>) {
  for k in 0..<37 { o[k] = i[k] &+ 1 }
}

// Heavy body, trip count 15 is below the minimum trip count of 16: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization3h15yys4SpanVys5UInt8VG_s07MutableC0VyAFGztF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func h15(_ i: Span<UInt8>, _ o: inout MutableSpan<UInt8>) {
  for k in 0..<15 {
    var x = i[k]; x = x ^ 0x5A; x = (x &<< 3) | (x &>> 5); x = x &+ 0x11
    x = x & 0x7F; x = x | 0x80; x = x ^ (x &>> 2); x = x &* 7; o[k] = ~x
  }
}

// Heavy body, trip count 16 meets the minimum trip count: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization3h16yys4SpanVys5UInt8VG_s07MutableC0VyAFGztF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func h16(_ i: Span<UInt8>, _ o: inout MutableSpan<UInt8>) {
  for k in 0..<16 {
    var x = i[k]; x = x ^ 0x5A; x = (x &<< 3) | (x &>> 5); x = x &+ 0x11
    x = x & 0x7F; x = x | 0x80; x = x ^ (x &>> 2); x = x &* 7; o[k] = ~x
  }
}

// Runtime trip count behind a minimum-iteration guard: vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization6rt_subyys4SpanVys5UInt8VG_s07MutableD0VyAFGzSitF"(
// CHECK:         vector.body:
// CHECK:       {{^}}}
public func rt_sub(_ i: Span<UInt8>, _ o: inout MutableSpan<UInt8>, _ n: Int) {
  for k in 0..<n { o[k] = i[k] &+ 1 }
}

// Table lookup (gather): data-dependent loads keep the loop scalar.

// 256-entry byte table, trip count 64: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization15g1_byteTable256yys4SpanVys5UInt8VG_s07MutableE0VyAFGzAGtF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func g1_byteTable256(
  _ inp: Span<UInt8>,
  _ out: inout MutableSpan<UInt8>,
  _ table: Span<UInt8>
) {
  for idx in 0..<64 {
    out[idx] = table[Int(inp[idx])]
  }
}

// 16-entry nibble table: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization16g2_nibbleTable16yys4SpanVys5UInt8VG_s07MutableE0VyAFGzAGtF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func g2_nibbleTable16(
  _ inp: Span<UInt8>,
  _ out: inout MutableSpan<UInt8>,
  _ table: Span<UInt8>
) {
  for idx in 0..<64 {
    out[idx] = table[Int(inp[idx] & 0x0F)]
  }
}

// Table lookup feeding a heavy body: not vectorized.
// CHECK-LABEL: define {{.*}} @"$s17autovectorization20g3_byteTable256Heavyyys4SpanVys5UInt8VG_s07MutableF0VyAFGzAGtF"(
// CHECK-NOT:     {{<[0-9]+ x }}
// CHECK:       {{^}}}
public func g3_byteTable256Heavy(
  _ inp: Span<UInt8>,
  _ out: inout MutableSpan<UInt8>,
  _ table: Span<UInt8>
) {
  for idx in 0..<64 {
    var x = table[Int(inp[idx])]
    x = x ^ 0x5A
    x = (x &<< 3) | (x &>> 5)
    x = x &+ 0x11
    x = x & 0x7F
    x = x | 0x80
    x = x ^ (x &>> 2)
    x = x &* 7
    out[idx] = ~x
  }
}
