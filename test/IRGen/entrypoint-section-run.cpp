// RUN: %empty-directory(%t)
// RUN: %clang %s -isysroot %sdk -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-build-swift %S/Inputs/at-main-struct-simple.swift -O -parse-as-library -emit-library -o %t/libHowdy.dylib -module-name Howdy
// RUN: %target-run %t/main %t/libHowdy.dylib | %FileCheck %s

// REQUIRES: OS=macosx,CPU=x86_64
// REQUIRES: executable_test
// UNSUPPORTED: remote_run

#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#include <stdio.h>
#include <string.h>

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
        reinterpret_cast<const mach_header_64 *>(_dyld_get_image_header(index));
    size_t size;
    auto *data = getsectiondata(header, "__TEXT", "__swift5_entry", &size);
    int32_t offset = *reinterpret_cast<int32_t *>(data);
    mainFunction = reinterpret_cast<MainFunction *>(
        reinterpret_cast<int64_t>(data) + offset);

    break;
  }
  if (!mainFunction) {
    printf("no function!");
    return 1;
  }
  mainFunction(argc, argv); // CHECK: howdy mundo
}
