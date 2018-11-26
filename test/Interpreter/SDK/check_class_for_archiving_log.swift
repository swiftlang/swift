// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name=_Test -import-objc-header %S/Inputs/check_class_for_archiving.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// This test doesn't use StdlibUnittest because it's primarily concerned with
// checking the presence and absence of output.

import Foundation

class SwiftClass {}

func checkArchiving(_ cls: AnyObject.Type) {
  NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(cls, operation: 0)
}
func checkUnarchiving(_ cls: AnyObject.Type) {
  NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(cls, operation: 1)
}

func mark(line: Int32 = #line) {
  NSLog("--%d--", line)
}

mark() // CHECK: --[[@LINE]]--
checkArchiving(SwiftClass.self)
mark() // CHECK-NEXT: --[[@LINE]]--


private class ArchivedTwice {}

checkArchiving(ArchivedTwice.self)
// CHECK-NEXT: Attempting to archive Swift class '_Test.({{.+}}).ArchivedTwice' with mangled runtime name '_TtC{{.+[0-9]+}}ArchivedTwice'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}ArchivedTwice)
// CHECK-NEXT: @objc(ABCArchivedTwice)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(ArchivedTwice.self)
mark() // CHECK-NEXT: --[[@LINE]]--

private class UnarchivedTwice {}

checkUnarchiving(UnarchivedTwice.self)
// CHECK-NEXT: Attempting to unarchive Swift class '_Test.({{.+}}).UnarchivedTwice' with mangled runtime name '_TtC{{.+[0-9]+}}UnarchivedTwice'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}UnarchivedTwice)
// CHECK-NEXT: @objc(ABCUnarchivedTwice)
mark() // CHECK-NEXT: --[[@LINE]]--
checkUnarchiving(UnarchivedTwice.self)
mark() // CHECK-NEXT: --[[@LINE]]--

private class ArchivedThenUnarchived {}

checkArchiving(ArchivedThenUnarchived.self)
// CHECK-NEXT: Attempting to archive Swift class '_Test.({{.+}}).ArchivedThenUnarchived' with mangled runtime name '_TtC{{.+[0-9]+}}ArchivedThenUnarchived'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}ArchivedThenUnarchived)
// CHECK-NEXT: @objc(ABCArchivedThenUnarchived)
mark() // CHECK-NEXT: --[[@LINE]]--
checkUnarchiving(ArchivedThenUnarchived.self)
mark() // CHECK-NEXT: --[[@LINE]]--

private class UnarchivedThenArchived {}

checkUnarchiving(UnarchivedThenArchived.self)
// CHECK-NEXT: Attempting to unarchive Swift class '_Test.({{.+}}).UnarchivedThenArchived' with mangled runtime name '_TtC{{.+[0-9]+}}UnarchivedThenArchived'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}UnarchivedThenArchived)
// CHECK-NEXT: @objc(ABCUnarchivedThenArchived)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(UnarchivedThenArchived.self)
mark() // CHECK-NEXT: --[[@LINE]]--

class Outer {
  class ArchivedTwice {}
  class UnarchivedTwice {}
  class ArchivedThenUnarchived {}
  class UnarchivedThenArchived {}
}

checkArchiving(Outer.ArchivedTwice.self)
// CHECK-NEXT: Attempting to archive Swift class '_Test.Outer.ArchivedTwice'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}ArchivedTwice)
// CHECK-NEXT: @objc(ABCArchivedTwice)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(Outer.ArchivedTwice.self)
mark() // CHECK-NEXT: --[[@LINE]]--

checkUnarchiving(Outer.UnarchivedTwice.self)
// CHECK-NEXT: Attempting to unarchive Swift class '_Test.Outer.UnarchivedTwice'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}UnarchivedTwice)
// CHECK-NEXT: @objc(ABCUnarchivedTwice)
mark() // CHECK-NEXT: --[[@LINE]]--
checkUnarchiving(Outer.UnarchivedTwice.self)
mark() // CHECK-NEXT: --[[@LINE]]--

checkArchiving(Outer.ArchivedThenUnarchived.self)
// CHECK-NEXT: Attempting to archive Swift class '_Test.Outer.ArchivedThenUnarchived'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}ArchivedThenUnarchived)
// CHECK-NEXT: @objc(ABCArchivedThenUnarchived)
mark() // CHECK-NEXT: --[[@LINE]]--
checkUnarchiving(Outer.ArchivedThenUnarchived.self)
mark() // CHECK-NEXT: --[[@LINE]]--

checkUnarchiving(Outer.UnarchivedThenArchived.self)
// CHECK-NEXT: Attempting to unarchive Swift class '_Test.Outer.UnarchivedThenArchived'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}UnarchivedThenArchived)
// CHECK-NEXT: @objc(ABCUnarchivedThenArchived)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(Outer.UnarchivedThenArchived.self)
mark() // CHECK-NEXT: --[[@LINE]]--


private class 日本語 {}

checkArchiving(日本語.self)
// CHECK-NEXT: Attempting to archive Swift class '_Test.({{.*}}).日本語'
// CHECK-NEXT: @objc(_TtC{{.+[0-9]+}}9日本語)
// CHECK-NEXT: @objc(ABCMyModel)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(日本語.self)
mark() // CHECK-NEXT: --[[@LINE]]--


class ArchivedTwiceGeneric<T> {}

checkArchiving(ArchivedTwiceGeneric<Int>.self)
// CHECK-NEXT: Attempting to archive generic Swift class '_Test.ArchivedTwiceGeneric<Swift.Int>' with mangled runtime name '_TtGC5_Test20ArchivedTwiceGenericSi_'
// CHECK-NEXT: NSKeyedUnarchiver.setClass(_:forClassName:)
// CHECK-SAME: _TtGC5_Test20ArchivedTwiceGenericSi_
// CHECK-NEXT: NSKeyedArchiver.setClassName(_:for:)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(ArchivedTwiceGeneric<Int>.self)
mark() // CHECK-NEXT: --[[@LINE]]--

checkArchiving(ArchivedTwiceGeneric<NSObject>.self)
// CHECK-NEXT: Attempting to archive generic Swift class '_Test.ArchivedTwiceGeneric<__C.NSObject>' with mangled runtime name '_TtGC5_Test20ArchivedTwiceGenericCSo8NSObject_'
// CHECK-NEXT: NSKeyedUnarchiver.setClass(_:forClassName:)
// CHECK-SAME: _TtGC5_Test20ArchivedTwiceGenericCSo8NSObject_
// CHECK-NEXT: NSKeyedArchiver.setClassName(_:for:)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(ArchivedTwiceGeneric<NSObject>.self)
mark() // CHECK-NEXT: --[[@LINE]]--

class UnarchivedTwiceGeneric<T> {}

checkUnarchiving(UnarchivedTwiceGeneric<Int>.self)
// CHECK-NEXT: Attempting to unarchive generic Swift class '_Test.UnarchivedTwiceGeneric<Swift.Int>' with mangled runtime name '_TtGC5_Test22UnarchivedTwiceGenericSi_'
// CHECK-NEXT: NSKeyedUnarchiver.setClass(_:forClassName:)
// CHECK-SAME: _TtGC5_Test22UnarchivedTwiceGenericSi_
// CHECK-NEXT: NSKeyedArchiver.setClassName(_:for:)
mark() // CHECK-NEXT: --[[@LINE]]--
checkUnarchiving(UnarchivedTwiceGeneric<Int>.self)
mark() // CHECK-NEXT: --[[@LINE]]--

class ArchivedThenUnarchivedGeneric<T> {}

checkArchiving(ArchivedThenUnarchivedGeneric<Int>.self)
// CHECK-NEXT: Attempting to archive generic Swift class '_Test.ArchivedThenUnarchivedGeneric<Swift.Int>' with mangled runtime name '_TtGC5_Test29ArchivedThenUnarchivedGenericSi_'
// CHECK-NEXT: NSKeyedUnarchiver.setClass(_:forClassName:)
// CHECK-SAME: _TtGC5_Test29ArchivedThenUnarchivedGenericSi_
// CHECK-NEXT: NSKeyedArchiver.setClassName(_:for:)
mark() // CHECK-NEXT: --[[@LINE]]--
checkUnarchiving(ArchivedThenUnarchivedGeneric<Int>.self)
mark() // CHECK-NEXT: --[[@LINE]]--

class UnarchivedThenArchivedGeneric<T> {}

checkUnarchiving(UnarchivedThenArchivedGeneric<Int>.self)
// CHECK-NEXT: Attempting to unarchive generic Swift class '_Test.UnarchivedThenArchivedGeneric<Swift.Int>' with mangled runtime name '_TtGC5_Test29UnarchivedThenArchivedGenericSi_'
// CHECK-NEXT: NSKeyedUnarchiver.setClass(_:forClassName:)
// CHECK-SAME: _TtGC5_Test29UnarchivedThenArchivedGenericSi_
// CHECK-NEXT: NSKeyedArchiver.setClassName(_:for:)
mark() // CHECK-NEXT: --[[@LINE]]--
checkArchiving(UnarchivedThenArchivedGeneric<Int>.self)
mark() // CHECK-NEXT: --[[@LINE]]--
