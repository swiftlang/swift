// <rdar://problem/15358345> Check that we always use PIC relocations on all
// platforms.

// RUN: %target-swift-frontend %s -module-name main -S -o - | FileCheck -check-prefix=%target-cpu %s

var global: Int = 0

public func use_global() -> Int {
  return global
}

// x86_64-LABEL: __TF4main10use_globalFT_Si:
// x86_64:         movq __Tv4main6globalSi(%rip), %rax

// i386-LABEL: __TF4main10use_globalFT_Si:
// i386:       [[PIC_BLOCK:^L.*\$pb]]:{{$}}
// i386:         popl [[PIC_REG:%[a-z]+]]
// i386:         movl __Tv4main6globalSi-[[PIC_BLOCK]]([[PIC_REG]]), {{%[a-z]+}}

// armv7-LABEL: __TF4main10use_globalFT_Si:
// armv7:         movw r0, :lower16:(__Tv4main6globalSi-([[PIC_0:L.*]]+8))
// armv7:         movt r0, :upper16:(__Tv4main6globalSi-([[PIC_0]]+8))
// armv7:       [[PIC_0]]:{{$}}
// armv7:         add r0, pc, r0
// armv7:         ldr r0, [r0]

// arm64-LABEL: __TF4main10use_globalFT_Si:
// arm64:         adrp [[REG1:x[0-9]+]], __Tv4main6globalSi@PAGE
// arm64:         add [[REG2:x[0-9]+]], [[REG1]], __Tv4main6globalSi@PAGEOFF
// arm64:         ldr {{x[0-9]+}}, {{\[}}[[REG2]]{{\]}}
