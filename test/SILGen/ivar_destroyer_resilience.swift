
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-emit-silgen -I %t -enable-library-evolution %s | %FileCheck %s

import resilient_struct

public class Base {}

public struct MyResilientInt {
  var i: Int

  public init(i: Int) { self.i = i }
}

public class NeedsIVarDetroyer : Base {
  var x = ResilientInt(i: 0)
}

public class DoesNotNeedIVarDestroyer : Base {
  var x = MyResilientInt(i: 0)
}

// CHECK-LABEL: sil_vtable NeedsIVarDetroyer {
// CHECK-NEXT: #Base.init!allocator: (Base.Type) -> () -> Base
// CHECK-NEXT: #NeedsIVarDetroyer.x!getter: (NeedsIVarDetroyer) -> () -> resilient_struct.ResilientInt
// CHECK-NEXT: #NeedsIVarDetroyer.x!setter: (NeedsIVarDetroyer) -> (resilient_struct.ResilientInt) -> ()
// CHECK-NEXT: #NeedsIVarDetroyer.x!modify: (NeedsIVarDetroyer) -> () -> ()
// CHECK-NEXT: #NeedsIVarDetroyer.deinit!deallocator
// CHECK-NEXT: #NeedsIVarDetroyer!ivardestroyer
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable DoesNotNeedIVarDestroyer {
// CHECK-NEXT: #Base.init!allocator: (Base.Type) -> () -> Base
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.x!getter: (DoesNotNeedIVarDestroyer) -> () -> MyResilientInt
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.x!setter: (DoesNotNeedIVarDestroyer) -> (MyResilientInt) -> ()
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.x!modify: (DoesNotNeedIVarDestroyer) -> () -> ()
// CHECK-NEXT: #DoesNotNeedIVarDestroyer.deinit!deallocator
// CHECK-NEXT: }
