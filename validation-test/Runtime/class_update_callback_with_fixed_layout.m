// Check that when Objective-C is first to touch a Swift class, it gives the
// Swift runtime a chance to update instance size and ivar offset metadata.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -emit-module -o %t/libResilient.dylib %S/Inputs/class-layout-from-objc/Resilient.swift -Xlinker -install_name -Xlinker @executable_path/libResilient.dylib -enable-library-evolution -DSMALL

// RUN: %target-clang -c %S/Inputs/class-layout-from-objc/OneWordSuperclass.m -fmodules -fobjc-arc -o %t/OneWordSuperclass.o
// RUN: %target-build-swift -emit-library -o %t/libClasses.dylib -emit-objc-header-path %t/Classes.h -I %t -I %S/Inputs/class-layout-from-objc/ %S/Inputs/class-layout-from-objc/Classes.swift %t/OneWordSuperclass.o -Xlinker -install_name -Xlinker @executable_path/libClasses.dylib -lResilient -L %t -Xfrontend -read-legacy-type-info-path=%S/Inputs/class-layout-from-objc/small.yaml
// RUN: %target-clang %s -I %S/Inputs/class-layout-from-objc/ -I %t -fmodules -fobjc-arc -o %t/main -lResilient -lClasses -L %t
// RUN: %target-codesign %t/main %t/libResilient.dylib %t/libClasses.dylib
// RUN: %target-run %t/main OLD %t/libResilient.dylib %t/libClasses.dylib

// RUN: %target-build-swift -emit-library -emit-module -o %t/libResilient.dylib %S/Inputs/class-layout-from-objc/Resilient.swift -Xlinker -install_name -Xlinker @executable_path/libResilient.dylib -enable-library-evolution -DBIG
// RUN: %target-codesign %t/libResilient.dylib
// RUN: %target-run %t/main NEW %t/libResilient.dylib %t/libClasses.dylib

// Try again when the class itself is also resilient.
// RUN: %target-build-swift -emit-library -o %t/libClasses.dylib -emit-objc-header-path %t/Classes.h -I %S/Inputs/class-layout-from-objc/ -I %t %S/Inputs/class-layout-from-objc/Classes.swift %t/OneWordSuperclass.o -Xlinker -install_name -Xlinker @executable_path/libClasses.dylib -lResilient -L %t -Xfrontend -read-legacy-type-info-path=%S/Inputs/class-layout-from-objc/big.yaml
// RUN: %target-codesign %t/libClasses.dylib
// RUN: %target-run %t/main OLD %t/libResilient.dylib %t/libClasses.dylib

// RUN: %target-build-swift -emit-library -emit-module -o %t/libResilient.dylib %S/Inputs/class-layout-from-objc/Resilient.swift -Xlinker -install_name -Xlinker @executable_path/libResilient.dylib -enable-library-evolution -DSMALL
// RUN: %target-codesign %t/libResilient.dylib
// RUN: %target-run %t/main NEW %t/libResilient.dylib %t/libClasses.dylib

// REQUIRES: executable_test
// REQUIRES: objc_interop

#import <objc/runtime.h>
#import <assert.h>
#import <dlfcn.h>
#import <stdbool.h>
#import <string.h>

#import "Classes.h"

void checkClass(Class c) {
  assert(c);

  size_t expectedSize = [c totalSize];
  size_t actualSize = class_getInstanceSize([c class]);
  NSLog(@"%@: expected size %zd, actual size %zd", c, expectedSize, actualSize);
  assert(expectedSize == actualSize);

  size_t expectedOffsetOfFirst = [c offsetOfFirst];
  size_t offsetOfFirst = ivar_getOffset(class_getInstanceVariable(c, "first"));
  NSLog(@"expected offset of 'first' %zd, actual %zd",
        expectedOffsetOfFirst, offsetOfFirst);
  assert(offsetOfFirst == expectedOffsetOfFirst);

  size_t offsetOfLast = ivar_getOffset(class_getInstanceVariable(c, "last"));
  NSLog(@"offset of 'last' %zd", offsetOfLast);
  assert(offsetOfLast == actualSize - sizeof(intptr_t));
}

int main(int argc, const char * const argv[]) {
  assert(argc > 1);

  if (!strcmp(argv[1], "OLD")) {
    ;
  } else if (!strcmp(argv[1], "NEW")) {
    // Only test the new behavior on a new enough libobjc.
    if (!dlsym(RTLD_NEXT, "_objc_realizeClassFromSwift")) {
      fprintf(stderr, "skipping evolution tests; OS too old\n");
      return EXIT_SUCCESS;
    }
  } else {
    fprintf(stderr, "usage: %s (OLD|NEW)\n", argv[0]);
    return EXIT_FAILURE;
  }

  @autoreleasepool {
    NSLog(@"%zd", class_getInstanceSize([OneWordSuperclass class]));
    checkClass([StaticClass class]);
    checkClass(objc_getClass("Classes.DynamicClass"));
    checkClass(objc_getClass("Classes.PureSwiftClass"));
  }
}
