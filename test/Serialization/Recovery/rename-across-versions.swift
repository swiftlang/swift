// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DNEW > %t/recovery.txt
// RUN: %FileCheck -check-prefix CHECK-RECOVERY %s < %t/recovery.txt
// RUN: %FileCheck -check-prefix NEGATIVE-RECOVERY %s < %t/recovery.txt

import RenameAcrossVersions

public func test(
  a: BeforeStruct,
  b: BeforeTypedef,
  c: BeforeWrappedTypedef
) {}

// CHECK-LABEL: func test(
// CHECK-SAME: a: BeforeStruct
// CHECK-SAME: b: BeforeTypedef
// CHECK-SAME: c: BeforeWrappedTypedef
// CHECK-SAME: )

// CHECK-RECOVERY-LABEL: func test(
// CHECK-RECOVERY-SAME: a: AfterStruct
// CHECK-RECOVERY-SAME: b: AfterTypedef
// CHECK-RECOVERY-SAME: c: AfterWrappedTypedef
// CHECK-RECOVERY-SAME: )

// Test replacements that look like renames.

// Please only include one parameter per function, so that we test that that one
// parameter is enough to get the function dropped from the recovery interface.
public func testReplacementA(_: BeforeReplacedType) {}

// CHECK-LABEL: func testReplacementA(_: BeforeReplacedType)

// NEGATIVE-RECOVERY-NOT: testReplacement
