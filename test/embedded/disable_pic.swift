// Check that -disable-position-independent-code switches from the PIC
// relocation model to the static relocation model in Embedded Swift, that PIC
// is the default, and that disabling PIC outside of Embedded Swift is an error.
// The relocation model is only observable on targets where the static model
// differs from PIC (e.g. x86_64 ELF); Mach-O is always position-independent.

// REQUIRES: swift_feature_Embedded
// REQUIRES: CODEGENERATOR=X86

// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -enable-experimental-feature Embedded -wmo -parse-stdlib %s -module-name main -S -o - | %FileCheck -check-prefix=PIC %s
// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -enable-experimental-feature Embedded -wmo -parse-stdlib -enable-position-independent-code %s -module-name main -S -o - | %FileCheck -check-prefix=PIC %s
// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -enable-experimental-feature Embedded -wmo -parse-stdlib -disable-position-independent-code %s -module-name main -S -o - | %FileCheck -check-prefix=STATIC %s

// Disabling PIC without Embedded Swift is rejected.
// RUN: not %target-swift-frontend -target x86_64-unknown-linux-gnu -parse-stdlib -disable-position-independent-code %s -module-name main -S -o - 2>&1 | %FileCheck -check-prefix=ERROR %s

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

// ERROR: -disable-position-independent-code is only applicable with embedded Swift
