// RUN: rm -rf %t && mkdir %t
// RUN: echo "int global() { return 42; }" | %clang -dynamiclib -o %t/libLinkMe.dylib -x c -
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t -module-name LinkMe -module-link-name LinkMe %S/../../Inputs/empty.swift

// RUN: %target-swift-frontend -interpret -DIMPORT %s -I %t -L %t 2>&1
// RUN: %target-swift-frontend -interpret -lLinkMe %s -L %t 2>&1
// RUN: not %target-swift-frontend -interpret -lLinkMe %s 2>&1

// RUN: %target-swift-frontend -interpret -lLinkMe -DUSE_DIRECTLY %s -L %t 2>&1
// RUN: not %target-swift-frontend -interpret -DUSE_DIRECTLY -lLinkMe %s 2>&1


// This is specifically testing autolinking for immediate mode. Please do not
// change it to use %target-build/%target-run
// REQUIRES: swift_interpreter


import Darwin
#if IMPORT
  import LinkMe
#endif


#if USE_DIRECTLY

@asmname("global")
func global() -> Int32

if global() != 42 {
  exit(EXIT_FAILURE)
}

#else

let RTLD_DEFAULT = UnsafeMutablePointer<Void>(bitPattern: -2)
if dlsym(RTLD_DEFAULT, "global") == nil {
  println(String.fromCString(dlerror())!)
  exit(EXIT_FAILURE)
}
#endif
