// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -parse-as-library %t/Lib.swift -module-name Lib -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-irgen -O -parse-as-library %t/App.sil -module-name App -I %t | %FileCheck %s

// rdar://189884332: App copies Lib's private Body.Storage field by field
// (retain_value_addr after LoadableByAddress). It must not use Storage's value
// witnesses, since Storage's metadata accessor isn't visible outside Lib.
// The field-wise copy is written in SIL so the test doesn't depend on whether
// the optimizer splits the copy of Body into per-field copies.

// CHECK-LABEL: define {{.*}}swiftcc void @"$s3App4copyy3Lib4BodyVyxG_AFtAFlF"(
// CHECK-NOT:     Storage{{.*}}Ma"(
// CHECK:         call ptr @"$s3Lib4BodyV7Storage33_{{[0-9A-F]+}}LLOyx_GlWOc"(
// CHECK:       }

//--- Lib.swift
public struct Body<W> {
  public var name: String?
  private enum Storage { case a(() -> Void); case b((Int) -> Void) }
  private var storage: Storage
}

//--- App.sil
sil_stage canonical

import Builtin
import Swift
import Lib

// public func copy<T>(_ body: Body<T>) -> (Body<T>, Body<T>) { (body, body) }
sil @$s3App4copyy3Lib4BodyVyxG_AFtAFlF : $@convention(thin) <T> (@guaranteed Body<T>) -> (@owned Body<T>, @owned Body<T>) {
bb0(%0 : $Body<T>):
  %1 = struct_extract %0, #Body.name
  %2 = struct_extract %0, #Body.storage
  retain_value %1
  retain_value %2
  retain_value %1
  retain_value %2
  %3 = tuple (%0, %0)
  return %3
}
