// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import -emit-module -emit-module-doc -o %t %s -import-objc-header %S/Inputs/enums.h -disable-objc-attr-requires-foundation-module -enable-library-evolution -module-name enums
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/enums.swiftmodule -typecheck -verify -emit-objc-header-path %t/enums.h -import-objc-header %S/Inputs/enums.h -disable-objc-attr-requires-foundation-module -enable-library-evolution
// RUN: %FileCheck %s < %t/enums.h
// RUN: %check-in-clang %t/enums.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/enums.h -include ctypes.h -include CoreFoundation.h

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, FrozenEnum, closed) {
@objc @frozen public enum FrozenEnum: Int {
  case yes
  case no
}

// CHECK-LABEL: typedef SWIFT_ENUM(NSInteger, NonFrozenEnum, open) {
@objc public enum NonFrozenEnum: Int {
  case yes
  case no
  case fileNotFound
}
