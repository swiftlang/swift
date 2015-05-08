// RUN: %target-run-simple-swift
// REQUIRES: OS=ios

import AssetsLibrary

// Test the enumerateGroupsWithTypes overload that accepts UInt32.
// This should compile and not crash at runtime.

let library = ALAssetsLibrary()
library.enumerateGroupsWithTypes(ALAssetsGroupAll,
  usingBlock: {(group: ALAssetsGroup!, stop: UnsafeMutablePointer<ObjCBool>) -> Void in
                print("Swift usingBlock")},
  failureBlock: {(error: NSError!) -> Void in
                  print("Swift failureBlock")})

