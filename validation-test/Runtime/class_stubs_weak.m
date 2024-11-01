// Check that Objective-C is able to use a resilient class stub emitted
// by the Swift compiler.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -emit-module -o %t/libfirst.dylib -emit-objc-header-path %t/first.h %S/Inputs/class-stubs-weak/first.swift -Xlinker -install_name -Xlinker @executable_path/libfirst.dylib -enable-library-evolution
// RUN: %target-build-swift -emit-library -o %t/libsecond.dylib -emit-objc-header-path %t/second.h -I %t %S/Inputs/class-stubs-weak/second.swift -Xlinker -install_name -Xlinker @executable_path/libsecond.dylib -lfirst -L %t -target %target-next-stable-abi-triple -DBEFORE
// RUN: cp %S/Inputs/class-stubs-weak/module.modulemap %t/

// Note: This is the just-built Clang, not the system Clang.
// RUN: xcrun -sdk %sdk %clang %s -I %t -L %t -fmodules -fobjc-arc -o %t/main -lfirst -lsecond -target %target-triple

// Now rebuild the library, omitting the weak-exported class
// RUN: %target-build-swift -emit-library -o %t/libsecond.dylib -I %t %S/Inputs/class-stubs-weak/second.swift -Xlinker -install_name -Xlinker @executable_path/libsecond.dylib -lfirst -L %t -target %target-next-stable-abi-triple

// RUN: %target-codesign %t/main %t/libfirst.dylib %t/libsecond.dylib
// RUN: %target-run %t/main %t/libfirst.dylib %t/libsecond.dylib

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_stable_abi

// REQUIRES: rdar62692550

#import <dlfcn.h>
#import <stdio.h>
#import "second.h"

@implementation DerivedClass (MyCategory)

- (int)instanceMethod {
  return [super instanceMethod] + 1;
}

+ (int)classMethod {
  return [super classMethod] + 1;
}

@end

int main(int argc, const char * const argv[]) {
  // Only test the new behavior on a new enough libobjc.
  if (!dlsym(RTLD_NEXT, "objc_loadClassref")) {
    fprintf(stderr, "skipping evolution tests; OS too old\n");
    return EXIT_SUCCESS;
  }

  Class cls = [DerivedClass class];
  if (cls) {
    printf("Class is not null");
    return EXIT_FAILURE;
  }

  printf("Class is null");
  return EXIT_SUCCESS;
}
