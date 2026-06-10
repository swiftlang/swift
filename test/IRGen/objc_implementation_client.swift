// Test doesn't pass on all platforms (rdar://101420862)
// REQUIRES: OS=macosx

// Note: This test employs test/IRGen/objc_implementation.swift as the
// implementation it's trying to use.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi -F %clang-importer-sdk-path/frameworks -import-objc-header %S/Inputs/objc_implementation.h -emit-module-path %t/objc_implementation.swiftmodule -target %target-future-triple %S/objc_implementation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t -I %S/Inputs/abi -F %clang-importer-sdk-path/frameworks %s -emit-ir -target %target-future-triple > %t.ir
// RUN: %FileCheck --input-file %t.ir %s
// REQUIRES: objc_interop

import objc_implementation

public func fn() {
  _ = ImplClass()
}

// Should emit a non-unique metadata accessor for this module's private use
// CHECK: define{{.*}} hidden {{.*}}@"$sSo9ImplClassCMa"
