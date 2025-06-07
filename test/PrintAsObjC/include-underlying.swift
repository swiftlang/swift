// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -typecheck -verify -emit-objc-header-path %t/emit.h -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/CoreGraphics-Bridging-Header.h -import-underlying-module -module-name CoreGraphics -bridging-header-directory-for-print ""
// RUN: %FileCheck -check-prefix=CHECK-DEFAULT %s < %t/emit.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -typecheck -verify -emit-objc-header-path %t/emit.h -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/CoreGraphics-Bridging-Header.h -import-underlying-module -module-name CoreGraphics -bridging-header-directory-for-print "Headers/PrivateHeaders/"
// RUN: %FileCheck -check-prefix=CHECK-DIR %s < %t/emit.h

@objc public class X: UIColor {
  @objc public func draw(_: UIColor) { }
}

// CHECK-DEFAULT: #import "CoreGraphics-Bridging-Header.h"
// CHECK-DIR: #import "Headers/PrivateHeaders/CoreGraphics-Bridging-Header.h"
