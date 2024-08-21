// RUN: %target-run-simple-swift(-import-bridging-header %S/Inputs/cdecl_implementation.h -D TOP_LEVEL_CODE -swift-version 5 -enable-experimental-feature CImplementation -target %target-stable-abi-triple) %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

@_objcImplementation @_cdecl("implFunc") public func implFunc(_ param: Int32) {
  print("implFunc(\(param))")
}

// `#if swift` to ignore the inactive branch's contents
#if swift(>=5.0) && TOP_LEVEL_CODE
implFunc(2023 - 34)
// CHECK: implFunc(1989)
#endif
