// <rdar://problem/15358345> Check that we always use PIC relocations on all
// platforms.

// RUN: %swift -triple x86_64-apple-darwin10 %s -parse-as-library -S | FileCheck -check-prefix=INTEL64 %s
// RUN: %swift -triple i386-apple-darwin10 %s -parse-as-library -S | FileCheck -check-prefix=INTEL32 %s
// RUN: %swift -triple armv7-apple-ios7 %s -parse-as-library -S | FileCheck -check-prefix=ARM32 %s

var global: Int = 0

def use_global() -> Int {
  return global
}

// INTEL64-LABEL: __T3pic10use_globalFT_Si:
// INTEL64:         movq __T3pic6globalSi(%rip), %rax

// INTEL32-LABEL: __T3pic10use_globalFT_Si:
// INTEL32:       [[PIC_BLOCK:^L.*\$pb]]:{{$}}
// INTEL32:         popl %eax
// INTEL32:         movl __T3pic6globalSi-[[PIC_BLOCK]](%eax), %ecx
// INTEL32:         movl (__T3pic6globalSi-[[PIC_BLOCK]])+4(%eax), %edx
// INTEL32:         movl %ecx, %eax

// ARM32-LABEL: __T3pic10use_globalFT_Si:
// ARM32:         movw r0, :lower16:(__T3pic6globalSi-([[PIC_0:L.*]]+8))
// ARM32:         movt r0, :upper16:(__T3pic6globalSi-([[PIC_0]]+8))
// ARM32:       [[PIC_0]]:{{$}}
// ARM32:         add r0, pc, r0
// ARM32:         ldr r1, [r0, #4]
// ARM32:         movw r0, :lower16:(__T3pic6globalSi-([[PIC_1:L.*]]+8))
// ARM32:         movt r0, :upper16:(__T3pic6globalSi-([[PIC_1]]+8))
// ARM32:       [[PIC_1]]:{{$}}
// ARM32:         ldr r0, [pc, r0]

