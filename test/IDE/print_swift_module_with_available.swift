// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/mymod.swiftmodule -module-name mymod %s
// RUN: %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=mymod -I %t -source-filename=%s > %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK1 < %t.syn.txt

// REQUIRES: OS=macosx

@available(OSX 10.11, iOS 8.0, *)
public class C1 {
}

@available(OSX 10.12, *)
public extension C1 {
  func ext_foo() {}
}

// CHECK1: @available(macOS 10.11, iOS 8.0, *)
// CHECK1-NEXT: public class C1 {

// CHECK1: @available(macOS 10.12, *)
// CHECK1-NEXT: extension C1 {
