// <rdar://problem/15358345> Check that we always use PIC relocations on all
// platforms.

// https://github.com/apple/swift/issues/54619
// XFAIL: OS=linux-android && CPU=aarch64
// XFAIL: OS=linux-androideabi && CPU=armv7
// UNSUPPORTED: OS=linux-gnu, CPU=wasm32

// RUN: %target-swift-frontend %s -module-name main -S -o - | %FileCheck -check-prefix=%target-cpu -check-prefix=%target-cpu-%target-abi -check-prefix=%target-cpu-%target-abi-%target-sdk-name %s

var global: Int = 0

public func use_global() -> Int {
  return global
}

// x86_64-LABEL: {{_?}}$s4main10use_globalSiyF:
// x86_64:         movq {{_?}}{{[(]?}}$s4main6globalSivp{{[)]?}}(%rip), %rax

// i386-LABEL: {{_?}}$s4main10use_globalSiyF:
// i386:       [[PIC_BLOCK:^L.*\$pb]]:{{$}}
// i386:         popl [[PIC_REG:%[a-z]+]]
// i386:         leal {{_?}}$s4main6globalSivp-[[PIC_BLOCK]]([[PIC_REG]]), {{%[a-z]+}}

// armv7-LABEL: {{_?}}$s4main10use_globalSiyF:
// Check for the runtime memory enforcement call. The global address may be
// materialized in a different register prior to that call.
// armv7-SYSV:         bl {{_?}}swift_beginAccess
// armv7-SYSV-iphoneos:         movw [[R_ADR:r.*]], :lower16:(_$s4main6globalSivp-([[PIC_0:L.*]]+4))
// armv7-SYSV-iphoneos:         movt [[R_ADR]], :upper16:(_$s4main6globalSivp-([[PIC_0]]+4))
// armv7-SYSV-iphoneos:       [[PIC_0]]:{{$}}
// armv7-SYSV-iphoneos:         ldr [[R_ADR]], {{\[}}[[R_ADR]]{{\]}}

// armv7-SYSV-android:          ldr [[R_ADR:r.*]], .LCPI[[PIC_0:[0-9]_[0-9]]]
// armv7-SYSV-android:        .LPC[[PIC_0]]:{{$}}
// armv7-SYSV-android:          add [[R_ADR]], pc
// armv7-SYSV-android:          bl {{_?}}swift_endAccess
// armv7-SYSV-android:        .LCPI[[PIC_0]]:{{$}}
// armv7-SYSV-android:     	   .long	($s4main6globalSivp)-(.LPC[[PIC_0]]+8)

// armv7-SYSV-linux:          ldr [[R_ADR:r.*]], .LCPI[[PIC_0:[0-9]_[0-9]]]
// armv7-SYSV-linux:        .LPC[[PIC_0]]:{{$}}
// armv7-SYSV-linux:          add [[R_ADR]], pc
// armv7-SYSV-linux:          bl {{_?}}swift_endAccess
// armv7-SYSV-linux:        .LCPI[[PIC_0]]:{{$}}
// armv7-SYSV-linux:     	   .long	($s4main6globalSivp)-(.LPC[[PIC_0]]+8)


// armv7s-LABEL: {{_?}}$s4main10use_globalSiyF:
// armv7s:         bl _swift_beginAccess
// armv7s:         movw [[R_ADR:r.*]], :lower16:(_$s4main6globalSivp-([[PIC_0:L.*]]+4))
// armv7s:         movt [[R_ADR]], :upper16:(_$s4main6globalSivp-([[PIC_0]]+4))
// armv7s:       [[PIC_0]]:{{$}}
// armv7s:         add [[R_ADR]], pc
// armv7s:         ldr [[R_ADR]], {{\[}}[[R_ADR]]{{\]}}

// armv7k-LABEL: {{_?}}$s4main10use_globalSiyF:
// armv7k:         bl _swift_beginAccess
// armv7k:        movw [[R_ADR:r.*]], :lower16:(_$s4main6globalSivp-([[PIC_0:L.*]]+4))
// armv7k:        movt [[R_ADR]], :upper16:(_$s4main6globalSivp-([[PIC_0]]+4))
// armv7k:      [[PIC_0]]:{{$}}
// armv7k:        add [[R_ADR]], pc
// armv7k:        ldr [[R_ADR]], {{\[}}[[R_ADR]]{{\]}}

// arm64-LABEL: {{_?}}$s4main10use_globalSiyF:
// arm64:        adrp [[REG3:x[0-9]+]], _$s4main6globalSivp@PAGE
// arm64:        str [[REG3]], [sp]
// arm64:        bl _swift_beginAccess
// arm64:        ldr [[REG4:x[0-9]+]], [sp]
// arm64:        ldr [[REG2:x[0-9]+]], {{\[}}[[REG4]], _$s4main6globalSivp@PAGEOFF
// arm64:        str [[REG2]], [sp, #16]
// arm64:        bl _swift_endAccess
// arm64:        ldr x0, [sp, #16]

// aarch64-LABEL: $s4main10use_globalSiyF:
// aarch64-SYSV:         bl swift_beginAccess
// aarch64-SYSV-linux:   adrp [[REG1:x[0-9]+]], ($s4main6globalSivp)
// aarch64-SYSV-android: adrp [[REG1:x[0-9]+]], ($s4main6globalSivp)
// aarch64-SYSV:         add [[REG1]], [[REG1]], :lo12:($s4main6globalSivp)
// aarch64-SYSV:         ldr [[REG2:x[0-9]+]], {{\[}}[[REG1]]{{\]}}
// aarch64-SYSV:         str [[REG2]], [sp]
// aarch64-SYSV:         bl swift_endAccess
// aarch64-SYSV:         ldr x0, [sp]

// aarch64-WIN:          adrp [[REG1:x[0-9]+]], $s4main6globalSivp
// aarch64-WIN:          add  [[REG1]], [[REG1]], :lo12:$s4main6globalSivp
// aarch64-WIN:          str  [[REG1]], [sp]
// aarch64-WIN:          adrp [[REG2:x[0-9]+]], __imp_swift_beginAccess
// aarch64-WIN:          ldr  [[REG2]], [[[REG2]], :lo12:__imp_swift_beginAccess]
// aarch64-WIN:          blr  [[REG2]]
// aarch64-WIN:          ldr  [[REG3:x[0-9]+]], [sp]
// aarch64-WIN:          ldr  [[REG4:x[0-9]+]], [[[REG3]]]

// The following checks are temporarily disabled. See rdar://problem/42909618
// arm64e-LABEL: _$s4main10use_globalSiyF:
// arm64e:         bl _swift_beginAccess
// arm64e:          _$s4main6globalSivp@PAGEOFF
// arm64e:         bl _swift_endAccess

// arm64_32-LABEL: _$s4main10use_globalSiyF:
// arm64_32:         adrp [[REG1:x[0-9]+]], _$s4main6globalSivp@PAGE
// arm64_32:         add [[REG1]], [[REG1]], _$s4main6globalSivp@PAGEOFF
// arm64_32:         bl _swift_beginAccess
// arm64_32:         ldr {{w[0-9]+}}, {{\[}}[[REG1]]{{\]}}

// powerpc64le-LABEL: {{_?}}$s4main10use_globalSiyF:
// powerpc64le:        bl swift_beginAccess
// powerpc64le:       addi 3, 3, ($s4main6globalSivp)@toc@l

// s390x-LABEL: $s4main10use_globalSiyF:
// s390x:        lgrl    %[[REG1:r[0-9]+]], ($s4main6globalSivp)
