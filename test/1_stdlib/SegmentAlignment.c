// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name %clang -c -arch %target-cpu %s -o %t/SegmentAlignment.o
// RUN: %target-build-swift %S/Inputs/SegmentAlignment.swift -Xlinker %t/SegmentAlignment.o -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: executable_test
// REQUIRES: CPU=armv7

// Verify 16K segment alignment on 32-bit iOS device.
// The linker sets this automatically on iOS 8+, 
// but we deploy to iOS 7.

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>

#if __LP64__
#define HeaderType struct mach_header_64
#else
#define HeaderType struct mach_header
#endif

// SegmentAlignment.swift import SpriteKit and calls Test().
void Test(void)
{
    for (int i = 0; i < _dyld_image_count(); i++) {
        const char *name = _dyld_get_image_name(i);
        if (strstr(name, "libswift") == 0) continue;

        unsigned long size;
        const struct mach_header *mhdr = _dyld_get_image_header(i);
        uint8_t *textAddress = 
            getsegmentdata((HeaderType *)mhdr, "__TEXT", &size);
        uint8_t *dataAddress = 
            getsegmentdata((HeaderType *)mhdr, "__DATA", &size);

        printf("%s %p %p\n", name, textAddress, dataAddress);
        assert((uintptr_t)textAddress % 0x4000 == 0);
        assert((uintptr_t)dataAddress % 0x4000 == 0);
    }

    printf("Flawless victory\n");
    // CHECK-DAG: libswiftSpriteKit.dylib
    // CHECK-DAG: libswiftUIKit.dylib
    // CHECK-DAG: libswiftCore.dylib
    // CHECK: Flawless victory
}
