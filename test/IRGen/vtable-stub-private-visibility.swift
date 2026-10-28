// RUN: %target-swift-frontend -O -module-name M -parse-as-library -emit-ir %s -o - | %FileCheck %s

// A specialization of a private method in an open class must not get an
// exported dead-method-stub alias: a specialization is never part of the
// module ABI (no vtable slot, no TBD entry), and an exported alias trips TBD
// validation ("symbol ... is in generated IR file, but not in TBD file"),
// which runs by default in asserts compilers. Because the existential
// specializer no longer inherits the original's subclass scope, the
// specialization keeps Private linkage, is not an externally-used symbol, and
// emitVTableStubs emits no alias for it at all. The original method's alias
// stays exported: TBD lists it, and derived-class vtables can reference a
// private base method cross-module.

// UNSUPPORTED: OS=windows-msvc
// (COFF keeps exported stubs to preserve dllexport behavior.)

public protocol BasePP {
  func processEvent(event: Int)
}
public protocol RefinedPP: BasePP {}

open class Store {
  private func dispatch(event: Int, processor: BasePP) {
    processor.processEvent(event: event)
  }
  public func run(_ processors: [RefinedPP], event: Int) {
    for p in processors {
      dispatch(event: event, processor: p)
    }
  }
}

// No stub alias is emitted for the specialization (note the specialization
// mangling suffix Tf4): it is not an externally-used symbol.
// CHECK-NOT: dispatch{{.*}}Tf4{{.*}} =
// ...while the original method's alias stays exported.
// CHECK: @{{.*}}dispatch{{.*}}BasePP_ptF" = alias void (), ptr @_swift_dead_method_stub
// CHECK-NOT: dispatch{{.*}}Tf4{{.*}} =
// CHECK: define {{(linkonce_odr )?}}hidden void @_swift_dead_method_stub(
// CHECK-NOT: dispatch{{.*}}Tf4{{.*}} =
