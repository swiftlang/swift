// RUN: %target-build-swift -emit-executable %s -import-objc-header %S/Inputs/clang_rt-helper.h -o %t

// REQUIRES: c_runtime

// Just make sure we can build and link successfully.

if isRunningOnFairlyRecentOS() {
  print("new!")
} else {
  print("old...")
}
