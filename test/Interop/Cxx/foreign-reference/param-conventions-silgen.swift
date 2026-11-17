// Swift, C and C++ functions borrow a foreign reference type parameter unless
// it is consumed, and a C++ method borrows `this`. A Swift function does not
// copy it on entry.

// RUN: %target-swift-emit-silgen %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple | %FileCheck %s

import ParamConventions

// CHECK-LABEL: sil [ossa] @$s4main5plainys5Int32VSo6SharedVF : $@convention(thin) (@guaranteed Shared) -> Int32 {
// CHECK:       bb0(%0 : @guaranteed $Shared):
// CHECK-NOT:     copy_value
// CHECK-LABEL: } // end sil function '$s4main5plainys5Int32VSo6SharedVF'
public func plain(_ s: Shared) -> Int32 {
  opaque()
  s.onRValue()
  return s.value() + s.virtualValue()
}

// A C++ method borrows `this`, even an rvalue one.
// CHECK-LABEL: sil {{.*}}[clang Shared.onRValue] @$sSo6SharedV8onRValueyyFTo : $@convention(cxx_method) (@guaranteed Shared) -> ()
// CHECK-LABEL: sil {{.*}}[clang Shared.value] @$sSo6SharedV5values5Int32VyFTo : $@convention(cxx_method) (@guaranteed Shared) -> Int32
// CHECK-LABEL: sil {{.*}}[clang Shared.virtualValue] @$sSo6SharedV12virtualValues5Int32VyFTo : $@convention(cxx_method) (@guaranteed Shared) -> Int32

// CHECK-LABEL: sil [ossa] @$s4main14borrowingParamys5Int32VSo6SharedVF : $@convention(thin) (@guaranteed Shared) -> Int32 {
// CHECK:       bb0(%0 : @noImplicitCopy @guaranteed $Shared):
public func borrowingParam(_ s: borrowing Shared) -> Int32 {
  return s.value()
}

// CHECK-LABEL: sil [ossa] @$s4main14consumingParamySo6SharedVADnF : $@convention(thin) (@owned Shared) -> @owned Shared {
// CHECK:       bb0(%0 : @noImplicitCopy @_eagerMove @owned $Shared):
public func consumingParam(_ s: consuming Shared) -> Shared {
  return s
}

// An optional reference already had the Swift conventions.
// CHECK-LABEL: sil [ossa] @$s4main13optionalParamys5Int32VSo6SharedVSgF : $@convention(thin) (@guaranteed Optional<Shared>) -> Int32 {
public func optionalParam(_ s: Shared?) -> Int32 {
  return s?.value() ?? 0
}

// CHECK-LABEL: sil [ossa] @$s4main12closureParamys5Int32VADSo6SharedVXE_AFtF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (@guaranteed Shared) -> Int32, @guaranteed Shared) -> Int32 {
public func closureParam(_ fn: (Shared) -> Int32, _ s: Shared) -> Int32 {
  return fn(s)
}

// An initializer and a setter take their parameters at +1.
public struct Holder {
  // CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s4main6HolderV6sharedSo6SharedVvs : $@convention(method) (@owned Shared, @inout Holder) -> () {
  public var shared: Shared

  // CHECK-LABEL: sil [ossa] @$s4main6HolderV6sharedACSo6SharedV_tcfC : $@convention(method) (@owned Shared, @thin Holder.Type) -> @owned Holder {
  public init(shared: Shared) {
    self.shared = shared
  }
}

extension Shared {
  // CHECK-LABEL: sil {{.*}}[ossa] @$sSo6SharedV4mainE5twices5Int32VyF : $@convention(method) (@guaranteed Shared) -> Int32 {
  // CHECK:       bb0(%0 : @guaranteed $Shared):
  // CHECK-NOT:     copy_value
  // CHECK-LABEL: } // end sil function '$sSo6SharedV4mainE5twices5Int32VyF'
  public func twice() -> Int32 {
    return value() + value()
  }
}

// An immortal reference is trivial.
// CHECK-LABEL: sil [ossa] @$s4main13immortalParamys5Int32VSo8ImmortalVF : $@convention(thin) (Immortal) -> Int32 {
public func immortalParam(_ i: Immortal) -> Int32 {
  return i.value()
}
// CHECK-LABEL: sil {{.*}}[clang Immortal.value] @$sSo8ImmortalV5values5Int32VyFTo : $@convention(cxx_method) (Immortal) -> Int32

// CHECK-LABEL: sil [ossa] @$s4main5callCyySo6SharedV_So8ImmortalVtF : $@convention(thin) (@guaranteed Shared, Immortal) -> () {
public func callC(_ s: Shared, _ i: Immortal) {
  takesPtr(s)
  takesNullablePtr(s)
  takesRef(s)
  takesConstRef(s)
  consumesPtr(s)
  consumesNullablePtr(s)
  _ = callWith({ $0.value() }, s)
  takesImmortal(i)
}

// CHECK-LABEL: sil {{.*}}[clang takesPtr] @$sSo8takesPtryySo6SharedVFTo : $@convention(c) (@guaranteed Shared) -> ()
// CHECK-LABEL: sil {{.*}}[clang takesNullablePtr] @$sSo16takesNullablePtryySo6SharedVSgFTo : $@convention(c) (@guaranteed Optional<Shared>) -> ()
// CHECK-LABEL: sil {{.*}}[clang takesRef] @$sSo8takesRefyySo6SharedVFTo : $@convention(c) (@guaranteed Shared) -> ()
// CHECK-LABEL: sil {{.*}}[clang takesConstRef] @$sSo13takesConstRefyySo6SharedVFTo : $@convention(c) (@guaranteed Shared) -> ()
// CHECK-LABEL: sil {{.*}}[clang consumesPtr] @$sSo11consumesPtryySo6SharedVnFTo : $@convention(c) (@owned Shared) -> ()
// CHECK-LABEL: sil {{.*}}[clang consumesNullablePtr] @$sSo19consumesNullablePtryySo6SharedVSgnFTo : $@convention(c) (@owned Optional<Shared>) -> ()

// A closure passed as a C function pointer borrows its argument too.
// CHECK-LABEL: sil private [ossa] @$s4main5callCyySo6SharedV_So8ImmortalVtFs5Int32VADcfU_ : $@convention(thin) (@guaranteed Shared) -> Int32 {
// CHECK-LABEL: sil private [thunk] [ossa] @$s4main5callCyySo6SharedV_So8ImmortalVtFs5Int32VADcfU_To : $@convention(c) (@guaranteed Shared) -> Int32 {
// CHECK:       bb0(%0 : @guaranteed $Shared):
// CHECK-LABEL: sil {{.*}}[clang callWith] @$sSo8callWithys5Int32VACSo6SharedVXC_AEtFTo : $@convention(c) (@convention(c) (@guaranteed Shared) -> Int32, @guaranteed Shared) -> Int32

// CHECK-LABEL: sil {{.*}}[clang takesImmortal] @$sSo13takesImmortalyySo0B0VFTo : $@convention(c) (Immortal) -> ()

// A named function or an `@c` function passed as a C function pointer borrows
// its argument, as a closure does. An `@c` function does not copy it on entry.
// CHECK-LABEL: sil hidden [asmname "cValue"] [ossa] @$s4main6cValueys5Int32VSo6SharedVFTo : $@convention(c) (@guaranteed Shared) -> Int32 {
// CHECK:       bb0(%0 : @guaranteed $Shared):
// CHECK-NOT:     copy_value
// CHECK-LABEL: } // end sil function '$s4main6cValueys5Int32VSo6SharedVFTo'

// CHECK-LABEL: sil [ossa] @$s4main9callNamedyySo6SharedVF : $@convention(thin) (@guaranteed Shared) -> () {
// CHECK:         function_ref @$s4main10namedValueys5Int32VSo6SharedVFTo : $@convention(c) (@guaranteed Shared) -> Int32
// CHECK:         function_ref @$s4main6cValueys5Int32VSo6SharedVFTo : $@convention(c) (@guaranteed Shared) -> Int32
// CHECK-LABEL: } // end sil function '$s4main9callNamedyySo6SharedVF'

// CHECK-LABEL: sil shared [thunk] [ossa] @$s4main10namedValueys5Int32VSo6SharedVFTo : $@convention(c) (@guaranteed Shared) -> Int32 {
// CHECK:       bb0(%0 : @guaranteed $Shared):
func namedValue(_ s: Shared) -> Int32 { return s.value() }
@c func cValue(_ s: Shared) -> Int32 { return s.value() }

public func callNamed(_ s: Shared) {
  _ = callWith(namedValue, s)
  _ = callWith(cValue, s)
}
