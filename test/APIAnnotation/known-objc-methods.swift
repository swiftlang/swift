// ObjectiveC
// RUN: %swift-ide-test -generate-api-annotation -o %t-ObjectiveC.apinotes ObjectiveC
// RUN: %swift-ide-test -check-api-annotation ObjectiveC %t-ObjectiveC.apinotes

// Foundation
// RUN: %swift-ide-test -generate-api-annotation -o %t-Foundation.apinotes Foundation
// RUN: %swift-ide-test -check-api-annotation Foundation %t-Foundation.apinotes

// AppKit
// RUN: %swift-ide-test -generate-api-annotation -o %t-AppKit.apinotes AppKit
// RUN: %swift-ide-test -check-api-annotation AppKit %t-AppKit.apinotes
// RUN: llvm-bcanalyzer %t-AppKit.apinotes | FileCheck %s
// NotificationCenter
// RUN: %swift-ide-test -generate-api-annotation -o %t-NotificationCenter.apinotes NotificationCenter
// RUN: %swift-ide-test -check-api-annotation NotificationCenter %t-NotificationCenter.apinotes

// UIKit
// RUN: %swift-ide-test -generate-api-annotation -o %t-UIKit.apinotes UIKit
// RUN: %swift-ide-test -check-api-annotation UIKit %t-UIKit.apinotes

// CHECK: Block ID #0 (BLOCKINFO_BLOCK)
// CHECK: Block ID #[[IDENTIFIER_BLOCK:[0-9]+]] (IDENTIFIER_BLOCK)
// CHECK: IDENTIFIER_DATA
// CHECK: Block ID #[[OBJC_CLASS_BLOCK:[0-9]+]] (OBJC_CLASS_BLOCK)
// CHECK: OBJC_CLASS_DATA
// CHECK: Block ID #[[OBJC_PROPERTY_BLOCK:[0-9]+]] (OBJC_PROPERTY_BLOCK)
// CHECK: OBJC_PROPERTY_DATA
// CHECK: Block ID #[[OBJC_METHOD_BLOCK:[0-9]+]] (OBJC_METHOD_BLOCK)
// CHECK: OBJC_METHOD_DATA
// CHECK: Block ID #[[OBJC_SELECTOR_BLOCK:[0-9]+]] (OBJC_SELECTOR_BLOCK)
// CHECK: OBJC_SELECTOR_DATA
