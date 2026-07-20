// The relocation model is controlled entirely by Clang and reused for Swift's
// own code generation. Two behaviors are exercised here:
//
//  * Hosted targets default to position-independent code; `-Xcc -fno-pic`
//    switches Swift to the static relocation model too. Disabling PIC is only
//    permitted in Embedded Swift.
//  * Embedded Swift targeting a bare-metal ("-none-" OS) triple is *not* forced
//    to be position-independent — it uses the target's default (static)
//    relocation model, and `-Xcc -fPIC` opts back in.
//
// REQUIRES: swift_feature_Embedded
// REQUIRES: CODEGENERATOR=X86

// Hosted target: PIC by default, overridable with -Xcc -fno-pic.
// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -enable-experimental-feature Embedded -wmo -parse-stdlib %s -module-name main -S -o - | %FileCheck -check-prefix=PIC %s
// RUN: %target-swift-frontend -target x86_64-unknown-linux-gnu -enable-experimental-feature Embedded -wmo -parse-stdlib -Xcc -fno-pic %s -module-name main -S -o - | %FileCheck -check-prefix=STATIC %s

// Bare-metal target: static by default (PIC is not forced), overridable with -Xcc -fPIC.
// RUN: %target-swift-frontend -target x86_64-unknown-none-elf -enable-experimental-feature Embedded -wmo -parse-stdlib %s -module-name main -S -o - | %FileCheck -check-prefix=STATIC %s
// RUN: %target-swift-frontend -target x86_64-unknown-none-elf -enable-experimental-feature Embedded -wmo -parse-stdlib -Xcc -fPIC %s -module-name main -S -o - | %FileCheck -check-prefix=PIC %s

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
