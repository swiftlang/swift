// FIXME: @objc enums from other files crash when switched. rdar://problem/19775284
// RUN: %target-build-swift -module-name main %s %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir | FileCheck %s
// XFAIL: *

// CHECK: buttbutt
func useFoo(x: Foo) {
  switch x {
  case .A:
    ()
  case .B:
    ()
  case .C:
    ()
  }
}
