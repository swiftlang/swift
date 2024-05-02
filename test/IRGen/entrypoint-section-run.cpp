// RUN: %empty-directory(%t)
// RUN: %target-clang %s -std=c++11 -isysroot %sdk -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-build-swift %S/Inputs/at-main-struct-simple.swift -O -parse-as-library -emit-library -o %t/libHowdy.dylib -module-name Howdy
// RUN: %target-codesign %t/libHowdy.dylib
// RUN: %target-run %t/main %t/libHowdy.dylib | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: executable_test
// UNSUPPORTED: remote_run
// UNSUPPORTED: asan

#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#include <stdio.h>
#include <string.h>
#include <ptrauth.h>

#if __POINTER_WIDTH__ == 64
using mach_header_platform = mach_header_64;
#else
using mach_header_platform = mach_header;
#endif

#if __has_feature(ptrauth_function_pointer_type_discrimination)
#define my_ptrauth_function_pointer_type_discriminator(__type) \
  __builtin_ptrauth_type_discriminator(__type)
#else
#define my_ptrauth_function_pointer_type_discriminator(__type) ((ptrauth_extra_data_t)0)
#endif

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("no argument!\n");
    return 1;
  }
  auto *dylibPath = argv[1];
  auto *handle = dlopen(dylibPath, RTLD_LAZY);
  if (!handle) {
    printf("no library!\n");
    return 1;
  }

  using MainFunction = void(int, char *[]);
  MainFunction *mainFunction = nullptr;
  for (int index = 0; index < _dyld_image_count(); ++index) {
    auto *imageName = _dyld_get_image_name(index);
    if (strcmp(dylibPath, imageName)) {
      printf("skipping %s\n", imageName);
      continue;
    }
    auto *header =
        reinterpret_cast<const mach_header_platform *>(_dyld_get_image_header(index));
    size_t size;
    auto *data = getsectiondata(header, "__TEXT", "__swift5_entry", &size);
    int32_t offset = *reinterpret_cast<int32_t *>(data);
    if (size >= 8) {
      auto flags = *(reinterpret_cast<int32_t *>(data) + 1);
      enum EntryPointFlags : unsigned {
        HasAtMainTypeFlag = 1 << 0,
      };
      printf("flags: %d\n", flags);
      bool isAtMainEntryPoint = flags & HasAtMainTypeFlag;
      if (!isAtMainEntryPoint) {
        printf("no @main entry point!\n");
        continue;
      }
    }
    mainFunction = reinterpret_cast<MainFunction *>(
      ptrauth_sign_unauthenticated(
          reinterpret_cast<void *>(
              reinterpret_cast<long>(data) + offset
          ),
          ptrauth_key_function_pointer,
          my_ptrauth_function_pointer_type_discriminator(MainFunction)
      )
    );

    break;
  }
  if (!mainFunction) {
    printf("no function!\n");
    return 1;
  }
  mainFunction(argc, argv); // CHECK: howdy mundo
}
