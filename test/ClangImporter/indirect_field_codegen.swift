// RUN: %target-swift-frontend -emit-silgen -I %S/Inputs/custom-modules %s | %FileCheck %s

// https://bugs.swift.org/browse/SR-10571

import IndirectFields

extension StructWithIndirectField2Copy {
  init(x: UInt32, y: UInt32) {
    self.init(.init(v: x + y))
  }

  init(s: StructWithIndirectField2) {
    self.init(x: s.x, y: s.y)
  }
}

// The names look complex because we assign names to unnamed unions/structs
// using a mangling scheme which the Swift demangler doesn't understand.

// CHECK-DAG: sil shared @$So24StructWithIndirectField2V34__Unnamed_union___Anonymous_field0V02__e10_struct___G7_field1V$x$getter : $@convention(c) (StructWithIndirectField2.__Unnamed_union___Anonymous_field0.__Unnamed_struct___Anonymous_field1) -> UInt32

// CHECK-DAG: sil shared @$So24StructWithIndirectField2V34__Unnamed_union___Anonymous_field0V02__e10_struct___G7_field1V$y$getter : $@convention(c) (StructWithIndirectField2.__Unnamed_union___Anonymous_field0.__Unnamed_struct___Anonymous_field1) -> UInt32

// CHECK-DAG: sil shared @$So28StructWithIndirectField2CopyV34__Unnamed_union___Anonymous_field0V02__f10_struct___H7_field1V$x$getter : $@convention(c) (StructWithIndirectField2Copy.__Unnamed_union___Anonymous_field0.__Unnamed_struct___Anonymous_field1) -> UInt32

// CHECK-DAG: sil shared @$So28StructWithIndirectField2CopyV34__Unnamed_union___Anonymous_field0V02__f10_struct___H7_field1V$x$setter : $@convention(c) (UInt32, @inout StructWithIndirectField2Copy.__Unnamed_union___Anonymous_field0.__Unnamed_struct___Anonymous_field1) -> ()

// CHECK-DAG: sil shared @$So28StructWithIndirectField2CopyV34__Unnamed_union___Anonymous_field0V02__f10_struct___H7_field1V$y$getter : $@convention(c) (StructWithIndirectField2Copy.__Unnamed_union___Anonymous_field0.__Unnamed_struct___Anonymous_field1) -> UInt32

// CHECK-DAG: sil shared @$So28StructWithIndirectField2CopyV34__Unnamed_union___Anonymous_field0V02__f10_struct___H7_field1V$y$setter : $@convention(c) (UInt32, @inout StructWithIndirectField2Copy.__Unnamed_union___Anonymous_field0.__Unnamed_struct___Anonymous_field1) -> ()

func test() -> UInt32 {
  var s = StructWithIndirectField2Copy(x: 1, y: 2)
  s.x = 10
  s.y = 11
  return s.x + s.y
}
