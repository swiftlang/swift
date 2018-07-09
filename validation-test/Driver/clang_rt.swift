// RUN: %target-build-swift -emit-executable %s -import-objc-header %S/Inputs/clang_rt-helper.h -o %t

// Just make sure we can build and link successfully.
if testNewOS() {
  print("new!")
} else {
  print("old...")
}
