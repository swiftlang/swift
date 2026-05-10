
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-emit-silgen -I %t -enable-library-evolution %s | %FileCheck %s

import resilient_struct

public class Base {}

public struct MyResilientInt {
  var i: Int

  public init(i: Int) { self.i = i }
}

public class NeedsIVarDestroyer : Base {
  var x = ResilientInt(i: 0)
}

public class DoesNotNeedIVarDestroyer : Base {
  var x = MyResilientInt(i: 0)
}

// CHECK-LABEL: sil_vtable NeedsIVarDestroyer {
// CHECK-NEXT: #Base.init!allocator: (Base.Type) -> () -> Base
// CHECK-NEXT: #NeedsIVarDestroyer.x!getter: (NeedsIVarDestroyer) -> () -> resilient_struct.ResilientInt
// CHECK-NEXT: #NeedsIVarDestroyer.x!setter: (NeedsIVarDestroyer) -> (resilient_struct.ResilientInt) -> ()
// CHECK-NEXT: #NeedsIVarDestroyer.x!modify: (NeedsIVarDestroyer) -> () -> ()
// CHECK-NEXT: #NeedsIVarDestroyer.deinit!deallocator
// CHECK-NEXT: #NeedsIVarDestroyer!ivardestroyer
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable DoesNotNeedIVarDestroyer {
// CHECK-NEXT: #Base.init!allocator: (Base.Type) -> () -> Base
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.x!getter: (DoesNotNeedIVarDestroyer) -> () -> MyResilientInt
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.x!setter: (DoesNotNeedIVarDestroyer) -> (MyResilientInt) -> ()
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.x!modify: (DoesNotNeedIVarDestroyer) -> () -> ()
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.deinit!deallocator
// CHECK-NEXT: }
