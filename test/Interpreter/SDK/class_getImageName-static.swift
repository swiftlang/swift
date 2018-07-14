// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -o %t/libSimpleNSObjectSubclass.dylib %S/Inputs/SimpleNSObjectSubclass.swift -Xlinker -install_name -Xlinker @executable_path/libSimpleNSObjectSubclass.dylib
// RUN: %target-codesign %t/libSimpleNSObjectSubclass.dylib

// RUN: %target-build-swift %s -o %t/main -lSimpleNSObjectSubclass -L%t -import-objc-header %S/Inputs/class_getImageName-static-helper.h
// RUN: %target-run %t/main %t/libSimpleNSObjectSubclass.dylib

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Darwin
import ObjectiveC
// import SimpleNSObjectSubclass // Deliberately omitted in favor of dynamic loads.

// Note: The following typealias uses AnyObject instead of AnyClass so that the
// function type is trivially bridgeable to Objective-C. (The representation of
// AnyClass is not the same as Objective-C's 'Class' type.)
typealias GetImageHook = @convention(c) (AnyObject, UnsafeMutablePointer<UnsafePointer<CChar>?>) -> ObjCBool
var hook: GetImageHook?

func checkThatSwiftHookWasNotInstalled() {
  // Check that the Swift hook did not get installed.
  guard let setHookPtr = dlsym(UnsafeMutableRawPointer(bitPattern: -2),
                               getHookName()) else {
    // If the version of the ObjC runtime we're using doesn't have the hook,
    // we're good.
    return
  }

  let setHook = unsafeBitCast(setHookPtr, to: (@convention(c) (GetImageHook, UnsafeMutablePointer<GetImageHook?>) -> Void).self)
  setHook({ hook!($0, $1) }, &hook)

  var info: Dl_info = .init()
  guard 0 != dladdr(unsafeBitCast(hook, to: UnsafeRawPointer.self), &info) else {
    fatalError("could not get dladdr info for objc_hook_getImageName")
  }

  precondition(String(cString: info.dli_fname).hasSuffix("libobjc.A.dylib"),
               "hook was replaced")
}

// It's important that this test does not register any Swift classes with the
// Objective-C runtime---that's where Swift sets up its custom hook, and we want
// to check the behavior /without/ that hook. That includes the buffer types for
// String and Array. Therefore, we get C strings directly from a bridging
// header.

guard let theClass = objc_getClass(getNameOfClassToFind()) as! AnyClass? else {
  fatalError("could not find class")
}

guard let imageName = class_getImageName(theClass) else {
  fatalError("could not find image")
}

checkThatSwiftHookWasNotInstalled()

// Okay, now we can use String.

precondition(String(cString: imageName).hasSuffix("libSimpleNSObjectSubclass.dylib"),
             "found wrong image")
