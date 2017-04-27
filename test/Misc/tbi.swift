// RUN: %target-build-swift -target arm64-apple-ios8.0 -target-cpu cyclone \
// RUN:   -O -S %s -parse-as-library -parse-stdlib -module-name Swift | \
// RUN:   %FileCheck --check-prefix=TBI %s

// RUN: %target-build-swift -target arm64-apple-ios7.0 -target-cpu cyclone \
// RUN:     -O -S %s -parse-as-library -parse-stdlib -module-name Swift | \
// RUN:   %FileCheck --check-prefix=NO_TBI %s

// REQUIRES: CODEGENERATOR=AArch64

// Verify that TBI is on by default in Swift on targets that support it. For our
// purposes this means iOS8.0 or later.

public enum Never {}

// NO_TBI-LABEL: .globl  __T0s7testTBIBi64_Bi64_1i_tF
// NO_TBI: __T0s7testTBIBi64_Bi64_1i_tF
// NO_TBI-NEXT: and
// NO_TBI-NEXT: ldr
// NO_TBI-NEXT: ret

// TBI-LABEL: .globl  __T0s7testTBIBi64_Bi64_1i_tF
// TBI: __T0s7testTBIBi64_Bi64_1i_tF:
// TBI-NEXT: ldr
// TBI-NEXT: ret

func testTBI(i: Builtin.Int64) -> Builtin.Int64 {

  // Some constants so we do not depend on the stdlib.
  let zero: Builtin.Int64 = Builtin.xor_Int64(i, i)
  let smallOne: Builtin.Int1 = Builtin.cmp_eq_Int64(zero, zero)
  let one: Builtin.Int64 = Builtin.zext_Int1_Int64(smallOne)
  let two: Builtin.Int64 = Builtin.add_Int64(one, one)
  let four: Builtin.Int64 = Builtin.add_Int64(two, two)
  let eight: Builtin.Int64 = Builtin.add_Int64(four, four)

  // 0xffff_ffff_ffff_ffff
  let allOnes: Builtin.Int64 = Builtin.sext_Int1_Int64(smallOne)
  // 0x00ff_ffff_ffff_ffff
  let constant: Builtin.Int64 = Builtin.lshr_Int64(allOnes, eight);

  let j = Builtin.and_Int64(i, constant)
  let rawValue: Builtin.RawPointer = Builtin.inttoptr_Int64(j)
  let zeroPtr: Builtin.RawPointer = Builtin.inttoptr_Int64(zero)
  let value: Builtin.Int1 = Builtin.cmp_eq_RawPointer(rawValue, zeroPtr)

  if value {
    // Make sure we optimize away this branch.
    Builtin.unreachable()
  }

  let x: Builtin.Int64 = Builtin.load(rawValue)
  return x
}
