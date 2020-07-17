// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name=_Test -import-objc-header %S/Inputs/check_class_for_archiving.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | grep 'check-prefix' > %t/prefix-option
// RUN: %target-run %t/a.out 2>&1 >/dev/null | %FileCheck `cat %t/prefix-option` %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// This test doesn't use StdlibUnittest because it's primarily concerned with
// checking the presence and absence of output.

import Foundation

// A tricky way to make the FileCheck tests conditional on the OS version.
if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  print("-check-prefix=CHECK")
} else {
  // Disable the checks for older OSes because of rdar://problem/50504765
  print("-check-prefix=DONT-CHECK")
  // Need at least one check, otherwise FileCheck will complain.
  // DONT-CHECK: {{.}}
}

class SwiftClass {}

func _check(_ label: String, _ cls: AnyObject.Type, _ op: CInt) {
  NSLog("--%@ start", label)
  NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(cls, operation: op)
  NSLog("--%@ end", label)
}
func checkArchiving(_ label: String, _ cls: AnyObject.Type) {
  _check(label, cls, 0)
}
func checkUnarchiving(_ label: String, _ cls: AnyObject.Type) {
  _check(label, cls, 1)
}


// CHECK-LABEL: --SwiftClass start
checkArchiving("SwiftClass", SwiftClass.self)
// CHECK-NEXT: --SwiftClass end


private class ArchivedTwice {}

// CHECK-LABEL: --ArchivedTwice1 start
checkArchiving("ArchivedTwice1", ArchivedTwice.self)
// CHECK: Attempting to archive Swift class '_Test.({{.+}}).ArchivedTwice' with {{.+}} runtime name '_TtC{{.+[0-9]+}}ArchivedTwice'
// CHECK: @objc(_TtC{{.+[0-9]+}}ArchivedTwice)

// CHECK-LABEL: --ArchivedTwice2 start
checkArchiving("ArchivedTwice2", ArchivedTwice.self)
// CHECK-NEXT: --ArchivedTwice2 end

private class UnarchivedTwice {}

// CHECK-LABEL: --UnarchivedTwice1 start
checkUnarchiving("UnarchivedTwice1", UnarchivedTwice.self)
// CHECK: Attempting to unarchive Swift class '_Test.({{.+}}).UnarchivedTwice' with {{.+}} runtime name '_TtC{{.+[0-9]+}}UnarchivedTwice'
// CHECK: @objc(_TtC{{.+[0-9]+}}UnarchivedTwice)

// CHECK-LABEL: --UnarchivedTwice2 start
checkUnarchiving("UnarchivedTwice2", UnarchivedTwice.self)
// CHECK-NEXT: --UnarchivedTwice2 end

private class ArchivedThenUnarchived {}

// CHECK-LABEL: --ArchivedThenUnarchived1 start
checkArchiving("ArchivedThenUnarchived1", ArchivedThenUnarchived.self)
// CHECK: Attempting to archive Swift class '_Test.({{.+}}).ArchivedThenUnarchived' with {{.+}} runtime name '_TtC{{.+[0-9]+}}ArchivedThenUnarchived'
// CHECK: @objc(_TtC{{.+[0-9]+}}ArchivedThenUnarchived)

// CHECK-LABEL: --ArchivedThenUnarchived2 start
checkUnarchiving("ArchivedThenUnarchived2", ArchivedThenUnarchived.self)
// CHECK-NEXT: --ArchivedThenUnarchived2 end

private class UnarchivedThenArchived {}

// CHECK-LABEL: --UnarchivedThenArchived1 start
checkUnarchiving("UnarchivedThenArchived1", UnarchivedThenArchived.self)
// CHECK: Attempting to unarchive Swift class '_Test.({{.+}}).UnarchivedThenArchived' with {{.+}} runtime name '_TtC{{.+[0-9]+}}UnarchivedThenArchived'
// CHECK: @objc(_TtC{{.+[0-9]+}}UnarchivedThenArchived)

// CHECK-LABEL: --UnarchivedThenArchived2 start
checkArchiving("UnarchivedThenArchived2", UnarchivedThenArchived.self)
// CHECK-NEXT: --UnarchivedThenArchived2 end

private class Outer {
  class ArchivedTwice {}
  class UnarchivedTwice {}
  class ArchivedThenUnarchived {}
  class UnarchivedThenArchived {}
}

// CHECK-LABEL: --Outer.ArchivedTwice1 start
checkArchiving("Outer.ArchivedTwice1", Outer.ArchivedTwice.self)
// CHECK: Attempting to archive Swift class '_Test.({{.+}}).Outer.ArchivedTwice'
// CHECK: @objc(_TtC{{.+[0-9]+}}ArchivedTwice)

// CHECK-LABEL: --Outer.ArchivedTwice2 start
checkArchiving("Outer.ArchivedTwice2", Outer.ArchivedTwice.self)
// CHECK-NEXT: --Outer.ArchivedTwice2 end

// CHECK-LABEL: --Outer.UnarchivedTwice1 start
checkUnarchiving("Outer.UnarchivedTwice1", Outer.UnarchivedTwice.self)
// CHECK: Attempting to unarchive Swift class '_Test.({{.+}}).Outer.UnarchivedTwice'
// CHECK: @objc(_TtC{{.+[0-9]+}}UnarchivedTwice)

// CHECK-LABEL: --Outer.UnarchivedTwice2 start
checkUnarchiving("Outer.UnarchivedTwice2", Outer.UnarchivedTwice.self)
// CHECK-NEXT: --Outer.UnarchivedTwice2 end

// CHECK-LABEL: --Outer.ArchivedThenUnarchived1 start
checkArchiving("Outer.ArchivedThenUnarchived1", Outer.ArchivedThenUnarchived.self)
// CHECK: Attempting to archive Swift class '_Test.({{.+}}).Outer.ArchivedThenUnarchived'
// CHECK: @objc(_TtC{{.+[0-9]+}}ArchivedThenUnarchived)

// CHECK-LABEL: --Outer.ArchivedThenUnarchived2 start
checkUnarchiving("Outer.ArchivedThenUnarchived2", Outer.ArchivedThenUnarchived.self)
// CHECK-NEXT: --Outer.ArchivedThenUnarchived2 end

// CHECK-LABEL: --Outer.UnarchivedThenArchived1 start
checkUnarchiving("Outer.UnarchivedThenArchived1", Outer.UnarchivedThenArchived.self)
// CHECK: Attempting to unarchive Swift class '_Test.({{.+}}).Outer.UnarchivedThenArchived'
// CHECK: @objc(_TtC{{.+[0-9]+}}UnarchivedThenArchived)

// CHECK-LABEL: --Outer.UnarchivedThenArchived2 start
checkArchiving("Outer.UnarchivedThenArchived2", Outer.UnarchivedThenArchived.self)
// CHECK-NEXT: --Outer.UnarchivedThenArchived2 end


private class 日本語 {}

// CHECK-LABEL: --Japanese1 start
checkArchiving("Japanese1", 日本語.self)
// CHECK: Attempting to archive Swift class '_Test.({{.*}}).日本語'

// CHECK-LABEL: --Japanese2 start
checkArchiving("Japanese2", 日本語.self)
// CHECK-NEXT: --Japanese2 end

func someFunction() {
  class LocalArchived: NSObject {}
  class LocalUnarchived: NSObject {}

  // CHECK-LABEL: --LocalArchived start
  checkArchiving("LocalArchived", LocalArchived.self)
  // CHECK: Attempting to archive Swift class '_Test.({{.+}}).LocalArchived'

  // CHECK-LABEL: --LocalUnarchived start
  checkUnarchiving("LocalUnarchived", LocalUnarchived.self)
  // CHECK: Attempting to unarchive Swift class '_Test.({{.+}}).LocalUnarchived'
}
someFunction()
