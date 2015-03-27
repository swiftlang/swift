// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t -module-name SomeModule -module-link-name functionNameDemangle %S/../../Inputs/empty.swift

// RUN: %target-swift-frontend -interpret -DIMPORT %s -I %t -L $(dirname %clang-include-dir)/lib 2>&1
// RUN: %target-swift-frontend -interpret -lfunctionNameDemangle %s -L $(dirname %clang-include-dir)/lib 2>&1
// RUN: not %target-swift-frontend -interpret -lfunctionNameDemangle %s 2>&1

// RUN: %target-swift-frontend -interpret -lfunctionNameDemangle -DUSE_DIRECTLY %s -L $(dirname %clang-include-dir)/lib 2>&1
// RUN: not %target-swift-frontend -interpret -DUSE_DIRECTLY -lfunctionNameDemangle %s 2>&1


// This is specifically testing autolinking for immediate mode. Please do not
// change it to use %target-build/%target-run
// REQUIRES: swift_interpreter


import Darwin
#if IMPORT
  import SomeModule
#endif


#if USE_DIRECTLY

@asmname("fnd_get_demangled_name")
func fnd_get_demangled_name(
  MangledName: UnsafePointer<CChar>,
  OutputBuffer: UnsafeMutablePointer<CChar>,
  Length: Int) -> Int

var outBuf = [CChar]()
if fnd_get_demangled_name("_", &outBuf, 0) != 0 {
  exit(EXIT_FAILURE)
}

#else

let RTLD_DEFAULT = UnsafeMutablePointer<Void>(bitPattern: -2)
if dlsym(RTLD_DEFAULT, "fnd_get_demangled_name") == nil {
  println(String.fromCString(dlerror())!)
  exit(EXIT_FAILURE)
}
#endif
