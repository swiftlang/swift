// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %s -module-name PublicSymbols
// RUN: %target-swift-frontend -parse-as-library %t/PublicSymbols.swiftmodule -typecheck -emit-objc-header-path %t/result.h -import-objc-header %S/../Inputs/empty.h -objc-header-is-public
// RUN: %FileCheck %s < %t/result.h

import Foundation

// CHECK: @protocol A
@objc public protocol A {}

// CHECK-NOT: @protocol B
@objc protocol B {}
