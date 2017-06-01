// <rdar://problem/15358345> Check that we always use PIC relocations on all
// platforms.

// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -module-name main -S -o - | %FileCheck -check-prefix=%target-cpu %s

// XFAIL: linux
// REQUIRES: rdar_32513284

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
// Check for the runtime memory enforcement call. The global address may be
// materialized in a different register prior to that call.
// armv7:         bl _swift_beginAccess
// armv7:         movw [[R_ADR:r.*]], :lower16:(__T04main6globalSiv-([[PIC_0:L.*]]+8))
// armv7:         movt [[R_ADR]], :upper16:(__T04main6globalSiv-([[PIC_0]]+8))
// armv7:       [[PIC_0]]:{{$}}
// armv7:         add [[R_ADR]], pc, [[R_ADR]]
// armv7:         ldr [[R_ADR]], {{\[}}[[R_ADR]]{{\]}}

// armv7s-LABEL: __T04main10use_globalSiyF:
// armv7s:         bl _swift_beginAccess
// armv7s:         movw [[R_ADR:r.*]], :lower16:(__T04main6globalSiv-([[PIC_0:L.*]]+8))
// armv7s:         movt [[R_ADR]], :upper16:(__T04main6globalSiv-([[PIC_0]]+8))
// armv7s:       [[PIC_0]]:{{$}}
// armv7s:         add [[R_ADR]], pc, [[R_ADR]]
// armv7s:         ldr [[R_ADR]], {{\[}}[[R_ADR]]{{\]}}

// armv7k-LABEL: __T04main10use_globalSiyF:
// armv7k:         bl _swift_beginAccess
// armv7k:        movw [[R_ADR:r.*]], :lower16:(__T04main6globalSiv-([[PIC_0:L.*]]+8))
// armv7k:        movt [[R_ADR]], :upper16:(__T04main6globalSiv-([[PIC_0]]+8))
// armv7k:      [[PIC_0]]:{{$}}
// armv7k:        add [[R_ADR]], pc, [[R_ADR]]
// armv7k:        ldr [[R_ADR]], {{\[}}[[R_ADR]]{{\]}}

// arm64-LABEL: __T04main10use_globalSiyF:
// arm64:         bl _swift_beginAccess
// arm64:         adrp [[REG1:x[0-9]+]], __T04main6globalSiv@PAGE
// arm64:         add [[REG2:x[0-9]+]], [[REG1]], __T04main6globalSiv@PAGEOFF
// arm64:         ldr {{x[0-9]+}}, {{\[}}[[REG2]]{{\]}}
