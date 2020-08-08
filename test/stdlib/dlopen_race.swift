// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -o %t/dlopen_race.dylib %S/Inputs/dlopen_race_dylib.swift
// RUN: %target-build-swift -o %t/dlopen_race %s
// RUN: %target-codesign %t/dlopen_race %t/dlopen_race.dylib
// RUN: %target-run %t/dlopen_race %t/dlopen_race.dylib
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest

import Darwin

var DlopenRaceTests = TestSuite("DlopenRace")

typealias add_image_callback = @convention(c) (UnsafeRawPointer?, Int) -> Void
func register_func_for_add_image(_ f: add_image_callback) {
  let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: -2)
  let _dyld_register_func_for_add_image_ptr =
    dlsym(RTLD_DEFAULT, "_dyld_register_func_for_add_image")
  expectNotNil(_dyld_register_func_for_add_image_ptr)

  typealias _dyld_register_func_for_add_image_func =
    @convention(c) (add_image_callback) -> Void
  let _dyld_register_func_for_add_image = unsafeBitCast(
    _dyld_register_func_for_add_image_ptr,
    to: _dyld_register_func_for_add_image_func.self)
  _dyld_register_func_for_add_image(f)
}

// Make sure Swift doesn't register newly opened images before ObjC is ready
// for them to be used. rdar://problem/49742015
var add_image_count = 0
DlopenRaceTests.test("race") {
  // This test is expected to fail unless the ObjC notification is supported.
  let RTLD_DEFAULT = UnsafeMutableRawPointer(bitPattern: -2)
  let objc_addLoadImageFunc = dlsym(RTLD_DEFAULT, "objc_addLoadImageFunc");
  if objc_addLoadImageFunc == nil { return }
  
  register_func_for_add_image({ header, slide in
    // The protocol conformance check in the print call is enough to trigger
    // ObjC class initialization in the newly opened image if Swift has
    // registered the conformance records in that image. While we would be
    // unlikely to make this sort of call directly in the callback in a real
    // program, it could happen at this time in another thread.
    print(header, slide)
    add_image_count += 1
  })
  
  let dylibPath = CommandLine.arguments.last!
  
  let beforeCount = add_image_count
  let handle = dlopen(dylibPath, RTLD_LAZY)
  expectNotNil(handle, String(cString: dlerror()))
  expectEqual(add_image_count, beforeCount + 1)
}

runAllTests()
