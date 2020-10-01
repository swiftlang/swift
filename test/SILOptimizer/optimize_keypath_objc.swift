// RUN: %empty-directory(%t) 
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -primary-file %s -O -sil-verify-all -emit-sil >%t/output.sil
// RUN: %FileCheck %s < %t/output.sil

// REQUIRES: objc_interop

import Foundation

class Foo: NSObject {
    @objc(doesIndeedHaveAKVCString) var hasKVCString: Int = 0

    var noKVCString: Int? = 0
}

// CHECK-LABEL: sil hidden @{{.*}}21optimize_keypath_objc12hasKVCString
func hasKVCString() -> String? {
    // CHECK-NOT:  = keypath
    // CHECK:      string_literal utf8 "doesIndeedHaveAKVCString"
    // CHECK-NOT:  = keypath
    // CHECK:      [[RESULT:%.*]] = enum $Optional<String>, #Optional.some
    return (\Foo.hasKVCString)._kvcKeyPathString
}

// CHECK-LABEL: sil hidden @{{.*}}21optimize_keypath_objc11noKVCString
func noKVCString() -> String? {
    // CHECK-NOT:  = keypath
    // CHECK:      [[RESULT:%.*]] = enum $Optional<String>, #Optional.none
    return (\Foo.noKVCString)._kvcKeyPathString
}
