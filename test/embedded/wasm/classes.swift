// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift -c %t/check.swift -parse-as-library -target wasm32-unknown-none-wasm \
// RUN:   -resource-dir %test-resource-dir \
// RUN:   -enable-experimental-feature Embedded -Xcc -fdeclspec -disable-stack-protector \
// RUN:   -o %t/check.o
// RUN: %clang -target wasm32-unknown-none-wasm %t/check.o %t/rt.c -nostdlib \
// RUN:   -I %swift_src_root/stdlib/public/EmbeddedPlatform -o %t/check.wasm
// RUN: %target-run %t/check.wasm
// REQUIRES: executable_test
// REQUIRES: CPU=wasm32
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

// This test pins the freestanding wasm32-unknown-none-wasm target (-nostdlib,
// its own rt.c and _start). The Emscripten triple has no freestanding
// configuration; it is always hosted and launched through emcc's JS glue.
// UNSUPPORTED: OS=emscripten

//--- rt.c

#include <stddef.h>
#include <stdint.h>

int putchar(int c) { return c; }
void free(void *ptr) {}
void *memmove(void *dest, const void *src, size_t n) {
    return __builtin_memmove(dest, src, n);
}

int posix_memalign(void **memptr, size_t alignment, size_t size) {
    uintptr_t mem = __builtin_wasm_memory_grow(0, (size + 0xffff) / 0x10000);
    if (mem == -1) {
        return -1;
    }
    *memptr = (void *)(mem * 0x10000);
    *memptr = (void *)(((uintptr_t)*memptr + alignment - 1) & -alignment);
    return 0;
}

// The platform abstraction layer hooks (see swift/EmbeddedPlatform.h) that
// the embedded standard library calls, implemented in terms of the above.
//
// These are defined here on purpose rather than by linking the toolchain's
// libswiftEmbeddedPlatformPOSIX.a: this freestanding program provides its own
// platform layer, which covers the custom-platform configuration where
// clients implement the hooks themselves. Only the hooks this program
// references are implemented.
#include "swift/EmbeddedPlatform.h"

void *_swift_allocate(__swift_size_t alignment, __swift_size_t size,
                      swift_alloc_flags_t flags) {
    void *ptr = NULL;
    if (posix_memalign(&ptr, alignment ? alignment : sizeof(void *), size))
        return NULL;
    return ptr;
}

void _swift_deallocate(void *ptr, __swift_size_t alignment,
                       __swift_size_t size, swift_dealloc_flags_t flags) {
    free(ptr);
}

void *_swift_typedAllocate(__swift_size_t size, __swift_size_t alignment,
                           swift_alloc_flags_t flags,
                           __swift_typeid_t typeId) {
    return _swift_allocate(alignment, size, flags);
}

void _swift_typedDeallocate(void *ptr, __swift_size_t size,
                            __swift_size_t alignment,
                            swift_dealloc_flags_t flags,
                            __swift_typeid_t typeId) {
    free(ptr);
}

void _swift_lockStandardOutput(void) {}
void _swift_unlockStandardOutput(void) {}

__swift_size_t _swift_writeToStandardOutput(const unsigned char *chars,
                                            __swift_size_t count) {
    for (__swift_size_t i = 0; i != count; ++i)
        putchar(chars[i]);
    return count;
}

void _swift_reportErrorAt(const unsigned char *message,
                          __swift_size_t messageCount,
                          const unsigned char *fileName,
                          __swift_size_t fileNameCount, __swift_size_t line,
                          __swift_options_t flags) {}

//--- check.swift

class Foo {}

@_cdecl("_start")
func main() {
  _ = Foo()
}
