// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %gyb %t/HugeTables.swift.gyb -o %t/HugeTables.swift

// RUN: %target-build-swift -target %target-swift-6.2-abi-triple -O -wmo -parse-as-library -module-name Tables -emit-module -emit-module-path %t/Tables.swiftmodule %t/Tables.swift %t/HugeTables.swift -c -o %t/Tables.o
// RUN: %target-build-swift -target %target-swift-6.2-abi-triple -O -wmo -parse-as-library -module-name Client -I %t -I %t/CTable %t/Client.swift -emit-ir -o %t/Client.ll
// RUN: %FileCheck %s --input-file=%t/Client.ll -DINT=i%target-ptrsize

// RUN: %target-clang -x c -O2 -c %t/CTable/ctable.c -o %t/ctable.o
// RUN: %target-build-swift -target %target-swift-6.2-abi-triple -O -wmo -parse-as-library -module-name Client -I %t -I %t/CTable %t/Client.swift -c -o %t/Client.o
// RUN: %target-build-swift -target %target-swift-6.2-abi-triple -O -module-name Main %t/main.swift %t/Client.o %t/Tables.o %t/ctable.o -o %t/a.out
// RUN: llvm-objdump -d --no-show-raw-insn --no-leading-addr %t/a.out | %FileCheck %s --check-prefix=CHECK-ASM %if CPU=arm64 || CPU=arm64e || CPU=aarch64 %{ --check-prefix=CHECK-ASM-ARM64 %}

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// Lock down how free table lookups are compared to a lookup in a C table, for tables in the same
// module and in another module. "Free" means the same IR as the C lookup, and no call in the
// linked machine code.

// Each lookup masks its index via the AND operator, to the table's size.
// That folds away Swift's bounds checks, so the lookups compare to C's, which has none.

// A failure might be an improvement in test cases that are currently not free.
// In such a case, flip the test to ensure the improvement is locked in.

//--- CTable/module.modulemap
module CTable {
  header "ctable.h"
  export *
}

//--- CTable/ctable.h
#include <stddef.h>
#include <stdint.h>

extern const uint8_t cTable[16];

static inline uint8_t cTableLookup(size_t i) {
  return cTable[i];
}

//--- CTable/ctable.c
#include "ctable.h"

const uint8_t cTable[16] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

//--- Tables.swift
public let arrayTable: [UInt8] = [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66]
public let inlineArrayTable: InlineArray<16, UInt8> = [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66]
public let staticStringTable: StaticString = "0123456789abcdef"

//--- HugeTables.swift.gyb
public let hugeInlineArrayTable: InlineArray<4097, UInt8> = [${', '.join(str(i % 128) for i in range(4097))}]

//--- main.swift

//--- Client.swift
import CTable
import Tables

let localArrayTable: [UInt8] = [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46]
let localInlineArrayTable: InlineArray<16, UInt8> = [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46]
let localStaticStringTable: StaticString = "0123456789ABCDEF"

// Free: the C baseline.
// CHECK-LABEL: define {{.*}} @"$s6Client7lookupCys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr @cTable, [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-ASM-LABEL: <{{_?}}$s6Client7lookupCys5UInt8VSiF>:
// CHECK-ASM-NOT:     {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
public func lookupC(_ i: Int) -> UInt8 {
  cTableLookup(i & 15)
}

// Free: the client emits its own copy of the string literal.
// CHECK-LABEL: define {{.*}} @"$s6Client18lookupStaticStringys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr @.str.16.0123456789abcdef, [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-ASM-LABEL: <{{_?}}$s6Client18lookupStaticStringys5UInt8VSiF>:
// CHECK-ASM-NOT:     {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
public func lookupStaticString(_ i: Int) -> UInt8 {
  staticStringTable.utf8Start[i & 15]
}

// Free: the client reads the other module's global directly.
// CHECK-LABEL: define {{.*}} @"$s6Client17lookupInlineArrayys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr @"$s6Tables16inlineArrayTables06InlineC0Vy$15_s5UInt8VGvp", [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-ASM-LABEL: <{{_?}}$s6Client17lookupInlineArrayys5UInt8VSiF>:
// CHECK-ASM-NOT:     {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
public func lookupInlineArray(_ i: Int) -> UInt8 {
  inlineArrayTable[i & 15]
}

// Free.
// CHECK-LABEL: define {{.*}} @"$s6Client23lookupLocalStaticStringys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr @.str.16.0123456789ABCDEF, [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-ASM-LABEL: <{{_?}}$s6Client23lookupLocalStaticStringys5UInt8VSiF>:
// CHECK-ASM-NOT:     {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
public func lookupLocalStaticString(_ i: Int) -> UInt8 {
  localStaticStringTable.utf8Start[i & 15]
}

// Free.
// CHECK-LABEL: define {{.*}} @"$s6Client22lookupLocalInlineArrayys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr @"$s6Client21localInlineArrayTables0cD0Vy$15_s5UInt8VGvp", [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-ASM-LABEL: <{{_?}}$s6Client22lookupLocalInlineArrayys5UInt8VSiF>:
// CHECK-ASM-NOT:     {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
public func lookupLocalInlineArray(_ i: Int) -> UInt8 {
  localInlineArrayTable[i & 15]
}

// Free (Almost): on arm64 the machine code has one `add` more than C.
// LLVM moves the offset of the elements behind the array's header from the table's address
// into the load, and arm64 has no load that adds both an index and an offset.
// CHECK-LABEL: define {{.*}} @"$s6Client16lookupLocalArrayys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr getelementptr inbounds nuw (i8, ptr @"$s6Client15localArrayTable_WZTv_{{r?}}", [[INT]] {{[0-9]+}}), [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-ASM-LABEL: <{{_?}}$s6Client16lookupLocalArrayys5UInt8VSiF>:
// CHECK-ASM-NOT:     {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
// CHECK-ASM-ARM64:   add x[[P:[0-9]+]], x{{[0-9]+}}, x{{[0-9]+}}
// CHECK-ASM-ARM64-NEXT: ldrb w0, [x[[P]], #0x{{[0-9a-f]+}}]
public func lookupLocalArray(_ i: Int) -> UInt8 {
  localArrayTable[i & 15]
}

// Not free: the client calls the addressor of the other module's global.
// An InlineArray of more than 4096 elements is address-only, and its global becomes statically
// initialized only after cross-module optimization. So at that point, its addressor still has
// the one-time initialization and isn't serialized.
// CHECK-LABEL: define {{.*}} @"$s6Client21lookupHugeInlineArrayys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[T:%.*]] = {{.*}}call swiftcc ptr @"$s6Tables20hugeInlineArrayTables0cD0Vy$4096_s5UInt8VGvau"()
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 4095
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr [[T]], [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-ASM-LABEL: <{{_?}}$s6Client21lookupHugeInlineArrayys5UInt8VSiF>:
// CHECK-ASM:         {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
public func lookupHugeInlineArray(_ i: Int) -> UInt8 {
  hugeInlineArrayTable[i & 4095]
}

// Not free: the client calls the addressor of the other module's global, then loads the
// array's buffer and its count, and checks the index against the count.
// An Array global becomes statically initialized only after cross-module optimization.
// So at that point, its addressor still has the one-time initialization and isn't serialized.
// CHECK-LABEL: define {{.*}} @"$s6Client11lookupArrayys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[G:%.*]] = {{.*}}call swiftcc ptr @"$s6Tables10arrayTableSays5UInt8VGvau"()
// CHECK-NEXT:    [[B:%.*]] = load ptr, ptr [[G]]
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[CP:%.*]] = getelementptr inbounds nuw i8, ptr [[B]], [[INT]] {{[0-9]+}}
// CHECK-NEXT:    [[C:%.*]] = load [[INT]], ptr [[CP]]
// CHECK-NEXT:    [[OK:%.*]] = icmp {{.*}} [[INT]] [[I]], [[C]]
// CHECK-NEXT:    br i1 [[OK]],
// CHECK:         [[E:%.*]] = getelementptr inbounds nuw i8, ptr [[B]], [[INT]] {{[0-9]+}}
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr [[E]], [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-ASM-LABEL: <{{_?}}$s6Client11lookupArrayys5UInt8VSiF>:
// CHECK-ASM:         {{ (bl|blr|blx|call|callq|jal|jalr|brasl) }}
public func lookupArray(_ i: Int) -> UInt8 {
  arrayTable[i & 15]
}
