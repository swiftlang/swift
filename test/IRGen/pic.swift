// <rdar://problem/15358345> Check that we always use PIC relocations on all
// platforms.

// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -module-name main -S -o - | %FileCheck -check-prefix=%target-cpu %s

// XFAIL: linux

var global: Int = 0

public func use_global() -> Int {
  return global
}

// x86_64-LABEL: __T04main10use_globalSiyF:
// x86_64:         movq __T04main6globalSiv(%rip), %rax

// i386-LABEL: __T04main10use_globalSiyF:
// i386:       [[PIC_BLOCK:^L.*\$pb]]:{{$}}
// i386:         popl [[PIC_REG:%[a-z]+]]
// i386:         movl __T04main6globalSiv-[[PIC_BLOCK]]([[PIC_REG]]), {{%[a-z]+}}

// armv7-LABEL: __T04main10use_globalSiyF:
// armv7:         movw r0, :lower16:(__T04main6globalSiv-([[PIC_0:L.*]]+8))
// armv7:         movt r0, :upper16:(__T04main6globalSiv-([[PIC_0]]+8))
// armv7:       [[PIC_0]]:{{$}}
// armv7:         add r0, pc, r0
// armv7:         ldr r0, [r0]

// armv7s-LABEL: __T04main10use_globalSiyF:
// armv7s:         movw r0, :lower16:(__T04main6globalSiv-([[PIC_0:L.*]]+8))
// armv7s:         movt r0, :upper16:(__T04main6globalSiv-([[PIC_0]]+8))
// armv7s:       [[PIC_0]]:{{$}}
// armv7s:         add r0, pc, r0
// armv7s:         ldr r0, [r0]

// armv7k-LABEL: __T04main10use_globalSiyF:
// armv7k:        movw r0, :lower16:(__T04main6globalSiv-([[PIC_0:L.*]]+8))
// armv7k:        movt r0, :upper16:(__T04main6globalSiv-([[PIC_0]]+8))
// armv7k:      [[PIC_0]]:{{$}}
// armv7k:        add r0, pc, r0
// armv7k:        ldr r0, [r0]

// arm64-LABEL: __T04main10use_globalSiyF:
// arm64:         adrp [[REG1:x[0-9]+]], __T04main6globalSiv@PAGE
// arm64:         add [[REG2:x[0-9]+]], [[REG1]], __T04main6globalSiv@PAGEOFF
// arm64:         ldr {{x[0-9]+}}, {{\[}}[[REG2]]{{\]}}
