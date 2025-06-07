// RUN: not %target-build-swift -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK-%target-os %s
// REQUIRES: objc_interop
// REQUIRES: executable_test

class IBActionWrapperTy {
  @IBAction func nullary() {}
  // CHECK-ios-NOT: attr_ibaction_ios.swift:[[@LINE-1]]
  // CHECK-macosx: attr_ibaction_ios.swift:[[@LINE-2]]:18: error: @IBAction methods must have 1 argument
  // CHECK-tvos-NOT: attr_ibaction_ios.swift:[[@LINE-3]]
  // CHECK-watchos-NOT: attr_ibaction_ios.swift:[[@LINE-4]]
  // CHECK-xros-NOT: attr_ibaction_ios.swift:[[@LINE-5]]

  @IBAction func unary(_: AnyObject) {}
  // CHECK-ios-NOT: attr_ibaction_ios.swift:[[@LINE-1]]
  // CHECK-macosx-NOT: attr_ibaction_ios.swift:[[@LINE-2]]
  // CHECK-tvos-NOT: attr_ibaction_ios.swift:[[@LINE-3]]
  // CHECK-watchos-NOT: attr_ibaction_ios.swift:[[@LINE-4]]
  // CHECK-xros-NOT: attr_ibaction_ios.swift:[[@LINE-5]]

  @IBAction func binary(_: AnyObject, _: AnyObject) {}
  // CHECK-ios-NOT: attr_ibaction_ios.swift:[[@LINE-1]]
  // CHECK-macosx: attr_ibaction_ios.swift:[[@LINE-2]]:18: error: @IBAction methods must have 1 argument
  // CHECK-tvos-NOT: attr_ibaction_ios.swift:[[@LINE-3]]
  // CHECK-watchos-NOT: attr_ibaction_ios.swift:[[@LINE-4]]
  // CHECK-xros-NOT: attr_ibaction_ios.swift:[[@LINE-5]]

  @IBAction func ternary(_: AnyObject, _: AnyObject, _: AnyObject) {}
  // CHECK-ios: attr_ibaction_ios.swift:[[@LINE-1]]:18: error: @IBAction methods must have at most 2 arguments
  // CHECK-macosx: attr_ibaction_ios.swift:[[@LINE-2]]:18: error: @IBAction methods must have 1 argument
  // CHECK-tvos: attr_ibaction_ios.swift:[[@LINE-3]]:18: error: @IBAction methods must have at most 2 arguments
  // CHECK-watchos: attr_ibaction_ios.swift:[[@LINE-4]]:18: error: @IBAction methods must have at most 2 arguments
  // CHECK-xros: attr_ibaction_ios.swift:[[@LINE-5]]:18: error: @IBAction methods must have at most 2 arguments
