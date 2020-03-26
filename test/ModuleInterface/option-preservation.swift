// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-library-evolution -emit-module-interface-path %t.swiftinterface -module-name t %s -emit-module -o /dev/null -Onone -enforce-exclusivity=unchecked -autolink-force-load
// RUN: %FileCheck %s < %t.swiftinterface -check-prefix=CHECK-SWIFTINTERFACE
//
// CHECK-SWIFTINTERFACE: swift-module-flags:
// CHECK-SWIFTINTERFACE-SAME: -enable-library-evolution
// CHECK-SWIFTINTERFACE-SAME: -Onone
// CHECK-SWIFTINTERFACE-SAME: -enforce-exclusivity=unchecked
// CHECK-SWIFTINTERFACE-SAME: -autolink-force-load

// Make sure flags show up when filelists are enabled

// RUN: %target-build-swift %s -driver-filelist-threshold=0 -emit-module-interface -o %t/foo -module-name foo -module-link-name fooCore -force-single-frontend-invocation -Ounchecked -enforce-exclusivity=unchecked -autolink-force-load 2>&1
// RUN: %FileCheck %s < %t/foo.swiftinterface --check-prefix CHECK-FILELIST-INTERFACE

// CHECK-FILELIST-INTERFACE: swift-module-flags:
// CHECK-FILELIST-INTERFACE-SAME: -target
// CHECK-FILELIST-INTERFACE-SAME: -autolink-force-load
// CHECK-FILELIST-INTERFACE-SAME: -module-link-name fooCore
// CHECK-FILELIST-INTERFACE-SAME: -enforce-exclusivity=unchecked
// CHECK-FILELIST-INTERFACE-SAME: -Ounchecked
// CHECK-FILELIST-INTERFACE-SAME: -module-name foo

public func foo() { }
