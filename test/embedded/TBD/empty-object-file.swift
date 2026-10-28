// An empty object file has no symbols for a TBD file to describe.

// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -c %s -o %t/Lib.o -parse-as-library -module-name Lib -enable-experimental-feature Embedded -emit-empty-object-file -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -c %s -o %t/Lib.o -parse-as-library -module-name Lib -emit-empty-object-file -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib 2>&1 | %FileCheck %s

// The empty object file alone is fine.
// RUN: %target-swift-frontend -c %s -o %t/Lib.o -parse-as-library -module-name Lib -enable-experimental-feature Embedded -emit-empty-object-file

// REQUIRES: swift_feature_Embedded

// CHECK: error: cannot emit a TBD file with '-emit-empty-object-file', which produces an object file without any symbols

public func f() {}
