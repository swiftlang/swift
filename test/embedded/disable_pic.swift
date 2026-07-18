// Position-independent vs. static code generation is controlled entirely by
// Clang (the relocation model Clang resolves is reused for Swift's own code
// generation), so `-Xcc -fno-pic` switches Swift to the static relocation
// model too. Disabling PIC is only permitted in Embedded Swift. The effect is
// only observable on targets where the static model differs from PIC (e.g.
// x86_64 ELF); Mach-O is always position-independent.

// REQUIRES: swift_feature_Embedded
// REQUIRES: CODEGENERATOR=X86

// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -enable-experimental-feature Embedded -wmo -parse-stdlib %s -module-name main -S -o - | %FileCheck -check-prefix=PIC %s
// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -enable-experimental-feature Embedded -wmo -parse-stdlib -Xcc -fno-pic %s -module-name main -S -o - | %FileCheck -check-prefix=STATIC %s

// Disabling PIC without Embedded Swift is rejected.
// RUN: not %target-swift-frontend -target x86_64-unknown-linux-gnu -parse-stdlib -Xcc -fno-pic %s -module-name main -S -o - 2>&1 | %FileCheck -check-prefix=ERROR %s

public var g: Builtin.Int64 = Builtin.zeroInitializer()

public func readG() -> Builtin.Int64 {
  return g
}

// The global is zero-initialized from top-level code. In PIC mode the store
// addresses the global RIP-relative; in the static relocation model it uses an
// absolute address.

// PIC-LABEL: {{^}}main:
// PIC:         movq $0, ($e4main1gBi64_vp)(%rip)

// STATIC-LABEL: {{^}}main:
// STATIC:         movq $0, ($e4main1gBi64_vp){{$}}

// ERROR: position-independent code can only be disabled with embedded Swift
