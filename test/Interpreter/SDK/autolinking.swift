// RUN: %empty-directory(%t)
// RUN: echo "int global() { return 42; }" | %clang -dynamiclib -o %t/libLinkMe.dylib -x c -
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t -module-name LinkMe -module-link-name LinkMe %S/../../Inputs/empty.swift

// RUN: %target-jit-run -DIMPORT %s -I %t -L %t 2>&1
// RUN: %target-jit-run -lLinkMe %s -L %t 2>&1
// RUN: not %target-jit-run -lLinkMe %s 2>&1

// RUN: %target-jit-run -lLinkMe -DUSE_DIRECTLY %s -L %t 2>&1
// RUN: not %target-jit-run -DUSE_DIRECTLY -lLinkMe %s 2>&1
// REQUIRES: executable_test


// This is specifically testing autolinking for immediate mode. Please do not
// change it to use %target-build/%target-run
// REQUIRES: swift_interpreter
// REQUIRES: OS=macosx


import Darwin
#if IMPORT
  import LinkMe
#endif


#if USE_DIRECTLY

@_silgen_name("global")
func global() -> Int32

if global() != 42 {
  exit(EXIT_FAILURE)
}

#else

let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: -2)
if dlsym(RTLD_DEFAULT, "global") == nil {
  if let err = dlerror() {
    print(String(cString: err))
  } else {
    print("Unknown dlsym error")
  }
  exit(EXIT_FAILURE)
}
#endif
