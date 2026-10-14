// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -target %target-cpu-apple-macosx50 -primary-file %t/caller.swift %t/narrowed.swift %t/broadened.swift

// RUN: %target-swift-frontend -typecheck -dump-availability-scopes -target %target-cpu-apple-macosx50 %t/narrowed.swift > %t/narrowed.dump 2>&1
// RUN: %FileCheck --strict-whitespace %t/narrowed.swift < %t/narrowed.dump
// RUN: %target-swift-frontend -typecheck -dump-availability-scopes -target %target-cpu-apple-macosx50 %t/broadened.swift > %t/broadened.dump 2>&1
// RUN: %FileCheck --strict-whitespace %t/broadened.swift < %t/broadened.dump

// REQUIRES: OS=macosx

//--- narrowed.swift
default @available(macOS 51, *)

// CHECK: {{^}}(root version=50
// CHECK-NEXT: {{^}}  (decl version=51 decl=defaulted()
public func defaulted() {}

// CHECK-NEXT: {{^}}  (decl version=52 decl=narrowed()
@available(macOS 52, *)
public func narrowed() {}

//--- broadened.swift
default @available(macOS 52, *)

// CHECK: {{^}}(root version=50
// CHECK-NEXT: {{^}}  (decl version=52 decl=broadened()
@available(macOS 51, *)
public func broadened() {}

//--- caller.swift
func unrefined() {
  defaulted()
  // expected-error@-1:3 {{'defaulted()' is only available in macOS 51 or newer}}
  // expected-note@-2:3 {{add 'if #available' version check}}
  // expected-note@-4:6 {{add '@available' attribute to enclosing global function}}
}

@available(macOS 51, *)
func refined() {
  defaulted()
  narrowed()
  // expected-error@-1:3 {{'narrowed()' is only available in macOS 52 or newer}}
  // expected-note@-2:3 {{add 'if #available' version check}}
  broadened()
  // expected-error@-1:3 {{'broadened()' is only available in macOS 52 or newer}}
  // expected-note@-2:3 {{add 'if #available' version check}}
}
