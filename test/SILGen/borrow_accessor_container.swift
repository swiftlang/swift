// RUN:%target-swift-frontend -emit-silgen %s -verify  -enable-experimental-feature BorrowAndMutateAccessors | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public struct Container<Element: ~Copyable >: ~Copyable {
  var _storage: UnsafeMutableBufferPointer<Element>
  var _count: Int

  var first: Element {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.baseAddress.unsafelyUnwrapped.pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      return &_storage.baseAddress.unsafelyUnwrapped.pointee
    }
  }

  public subscript(index: Int) -> Element {
    @_unsafeSelfDependentResult
    borrow {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return &_storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
  }
}

extension Container: Copyable where Element: Copyable {}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_container9ContainerVAARi_zrlE5firstxvb : $@convention(method) <Element where Element : ~Copyable> (@guaranteed Container<Element>) -> @guaranteed_address Element {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Container<Element>):
// CHECK:   [[REG1:%.*]] = copy_value [[REG0]]
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG1]]
// CHECK:   debug_value [[REG2]], let, name "self", argno 1
// CHECK:   [[REG4:%.*]] = begin_borrow [[REG2]]
// CHECK:   [[REG5:%.*]] = struct_extract [[REG0]], #Container._storage
// CHECK:   [[REG6:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<Element>>
// CHECK:   [[REG7:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG8:%.*]] = apply [[REG7]]<Element>([[REG5]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   store [[REG8]] to [trivial] [[REG6]]
// CHECK:   [[REG10:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   [[REG11:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG12:%.*]] = apply [[REG11]]<UnsafeMutablePointer<Element>>([[REG10]], [[REG6]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG13:%.*]] = load [trivial] [[REG10]]
// CHECK:   [[REG14:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvlu : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG15:%.*]] = apply [[REG14]]<Element>([[REG13]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG16:%.*]] = struct_extract [[REG15]], #UnsafePointer._rawValue
// CHECK:   [[REG17:%.*]] = pointer_to_address [[REG16]] to [strict] $*Element
// CHECK:   [[REG18:%.*]] = mark_dependence [unresolved] [[REG17]] on [[REG13]]
// CHECK:   [[REG19:%.*]] = begin_access [read] [unsafe] [[REG18]]
// CHECK:   [[REG20:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG19]]
// CHECK:   end_access [[REG19]]
// CHECK:   dealloc_stack [[REG10]]
// CHECK:   dealloc_stack [[REG6]]
// CHECK:   end_borrow [[REG4]]
// CHECK:   destroy_value [[REG2]]
// CHECK:   return [[REG20]]
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_container9ContainerVAARi_zrlE5firstxvz : $@convention(method) <Element where Element : ~Copyable> (@inout Container<Element>) -> @inout Element {
// CHECK: bb0([[REG0:%.*]] : $*Container<Element>):
// CHECK:   debug_value [[REG0]], var, name "self", argno 1, expr op_deref
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #Container._storage
// CHECK:   [[REG4:%.*]] = load [trivial] [[REG3]]
// CHECK:   [[REG5:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<Element>>
// CHECK:   [[REG6:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<Element>([[REG4]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   store [[REG7]] to [trivial] [[REG5]]
// CHECK:   [[REG9:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   [[REG10:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG11:%.*]] = apply [[REG10]]<UnsafeMutablePointer<Element>>([[REG9]], [[REG5]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG12:%.*]] = load [trivial] [[REG9]]
// CHECK:   [[REG13:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvau : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG14:%.*]] = apply [[REG13]]<Element>([[REG12]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG15:%.*]] = struct_extract [[REG14]], #UnsafeMutablePointer._rawValue
// CHECK:   [[REG16:%.*]] = pointer_to_address [[REG15]] to [strict] $*Element
// CHECK:   [[REG17:%.*]] = mark_dependence [unresolved] [[REG16]] on [[REG12]]
// CHECK:   [[REG18:%.*]] = begin_access [modify] [unsafe] [[REG17]]
// CHECK:   [[REG19:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG18]]
// CHECK:   end_access [[REG18]]
// CHECK:   dealloc_stack [[REG9]]
// CHECK:   dealloc_stack [[REG5]]
// CHECK:   return [[REG19]]
// CHECK: }

// CHECK: sil [ossa] @$s25borrow_accessor_container9ContainerVAARi_zrlEyxSicib : $@convention(method) <Element where Element : ~Copyable> (Int, @guaranteed Container<Element>) -> @guaranteed_address Element {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : @guaranteed $Container<Element>):
// CHECK:   [[REG3:%.*]] = copy_value [[REG1]]
// CHECK:   [[REG4:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG3]]
// CHECK:   [[REG6:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   [[REG7:%.*]] = begin_borrow [[REG4]]
// CHECK:   [[REG8:%.*]] = struct_extract [[REG7]], #Container._storage
// CHECK:   [[REG9:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG10:%.*]] = apply [[REG9]]<Element>([[REG8]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG11:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<Element>>
// CHECK:   store [[REG10]] to [trivial] [[REG11]]
// CHECK:   [[REG13:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   [[REG14:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG15:%.*]] = apply [[REG14]]<UnsafeMutablePointer<Element>>([[REG13]], [[REG11]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG16:%.*]] = load [trivial] [[REG13]]
// CHECK:   [[REG17:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   store [[REG16]] to [trivial] [[REG17]]
// CHECK:   [[REG19:%.*]] = function_ref @$ss8_PointerPsE8advanced2byxSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[REG20:%.*]] = apply [[REG19]]<UnsafeMutablePointer<Element>>([[REG6]], [[REG0]], [[REG17]]) : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   dealloc_stack [[REG17]]
// CHECK:   dealloc_stack [[REG13]]
// CHECK:   dealloc_stack [[REG11]]
// CHECK:   end_borrow [[REG7]]
// CHECK:   [[REG25:%.*]] = load [trivial] [[REG6]]
// CHECK:   [[REG26:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvlu : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG27:%.*]] = apply [[REG26]]<Element>([[REG25]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG28:%.*]] = struct_extract [[REG27]], #UnsafePointer._rawValue
// CHECK:   [[REG29:%.*]] = pointer_to_address [[REG28]] to [strict] $*Element
// CHECK:   [[REG30:%.*]] = mark_dependence [unresolved] [[REG29]] on [[REG25]]
// CHECK:   [[REG31:%.*]] = begin_access [read] [unsafe] [[REG30]]
// CHECK:   [[REG32:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG31]]
// CHECK:   end_access [[REG31]]
// CHECK:   dealloc_stack [[REG6]]
// CHECK:   destroy_value [[REG4]]
// CHECK:   return [[REG32]]
// CHECK: }

// CHECK: sil [ossa] @$s25borrow_accessor_container9ContainerVAARi_zrlEyxSiciz : $@convention(method) <Element where Element : ~Copyable> (Int, @inout Container<Element>) -> @inout Element {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*Container<Element>):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], var, name "self", argno 2, expr op_deref
// CHECK:   [[REG4:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG1]]
// CHECK:   [[REG5:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   [[REG6:%.*]] = begin_access [read] [unknown] [[REG4]]
// CHECK:   [[REG7:%.*]] = struct_element_addr [[REG6]], #Container._storage
// CHECK:   [[REG8:%.*]] = load [trivial] [[REG7]]
// CHECK:   end_access [[REG6]]
// CHECK:   [[REG10:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG11:%.*]] = apply [[REG10]]<Element>([[REG8]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG12:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<Element>>
// CHECK:   store [[REG11]] to [trivial] [[REG12]]
// CHECK:   [[REG14:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   [[REG15:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG16:%.*]] = apply [[REG15]]<UnsafeMutablePointer<Element>>([[REG14]], [[REG12]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG17:%.*]] = load [trivial] [[REG14]]
// CHECK:   [[REG18:%.*]] = alloc_stack $UnsafeMutablePointer<Element>
// CHECK:   store [[REG17]] to [trivial] [[REG18]]
// CHECK:   [[REG20:%.*]] = function_ref @$ss8_PointerPsE8advanced2byxSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[REG21:%.*]] = apply [[REG20]]<UnsafeMutablePointer<Element>>([[REG5]], [[REG0]], [[REG18]]) : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   dealloc_stack [[REG18]]
// CHECK:   dealloc_stack [[REG14]]
// CHECK:   dealloc_stack [[REG12]]
// CHECK:   [[REG25:%.*]] = load [trivial] [[REG5]]
// CHECK:   [[REG26:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvau : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG27:%.*]] = apply [[REG26]]<Element>([[REG25]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG28:%.*]] = struct_extract [[REG27]], #UnsafeMutablePointer._rawValue
// CHECK:   [[REG29:%.*]] = pointer_to_address [[REG28]] to [strict] $*Element
// CHECK:   [[REG30:%.*]] = mark_dependence [unresolved] [[REG29]] on [[REG25]]
// CHECK:   [[REG31:%.*]] = begin_access [modify] [unsafe] [[REG30]]
// CHECK:   [[REG32:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG31]]
// CHECK:   end_access [[REG31]]
// CHECK:   dealloc_stack [[REG5]]
// CHECK:   return [[REG32]]
// CHECK: }

public class Klass {}

public struct S {
  var _k: Klass
}

public struct CopyableContainer {
  var _storage: UnsafeMutableBufferPointer<S>
  var _count: Int

  var first: S {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.baseAddress.unsafelyUnwrapped.pointee
    }
  }

  public subscript(index: Int) -> Klass {
    @_unsafeSelfDependentResult
    borrow {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee._k
    }
    @_unsafeSelfDependentResult
    mutate {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return &_storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee._k
    }
  }
}

// CHECK: sil [ossa] @$s25borrow_accessor_container17CopyableContainerVyAA5KlassCSicib : $@convention(method) (Int, CopyableContainer) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $CopyableContainer):
// CHECK:   [[REG4:%.*]] = alloc_stack $UnsafeMutablePointer<S>
// CHECK:   [[REG5:%.*]] = struct_extract [[REG1]], #CopyableContainer._storage
// CHECK:   [[REG6:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<S>([[REG5]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG8:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<S>>
// CHECK:   store [[REG7]] to [trivial] [[REG8]]
// CHECK:   [[REG10:%.*]] = alloc_stack $UnsafeMutablePointer<S>
// CHECK:   [[REG11:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG12:%.*]] = apply [[REG11]]<UnsafeMutablePointer<S>>([[REG10]], [[REG8]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG13:%.*]] = load [trivial] [[REG10]]
// CHECK:   [[REG14:%.*]] = alloc_stack $UnsafeMutablePointer<S>
// CHECK:   store [[REG13]] to [trivial] [[REG14]]
// CHECK:   [[REG16:%.*]] = function_ref @$ss8_PointerPsE8advanced2byxSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[REG17:%.*]] = apply [[REG16]]<UnsafeMutablePointer<S>>([[REG4]], [[REG0]], [[REG14]]) : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   dealloc_stack [[REG14]]
// CHECK:   dealloc_stack [[REG10]]
// CHECK:   dealloc_stack [[REG8]]
// CHECK:   [[REG21:%.*]] = load [trivial] [[REG4]]
// CHECK:   [[REG22:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvlu : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG23:%.*]] = apply [[REG22]]<S>([[REG21]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG24:%.*]] = struct_extract [[REG23]], #UnsafePointer._rawValue
// CHECK:   [[REG25:%.*]] = pointer_to_address [[REG24]] to [strict] $*S
// CHECK:   [[REG26:%.*]] = mark_dependence [unresolved] [[REG25]] on [[REG21]]
// CHECK:   [[REG27:%.*]] = begin_access [read] [unsafe] [[REG26]]
// CHECK:   [[REG28:%.*]] = struct_element_addr [[REG27]], #S._k
// CHECK:   [[REG29:%.*]] = load_borrow [[REG28]]
// CHECK:   [[REG31:%.*]] = unchecked_ownership_conversion [[REG29]], @guaranteed to @unowned
// CHECK:   [[REG32:%.*]] = unchecked_ownership_conversion [[REG31]], @unowned to @guaranteed
// CHECK:   end_access [[REG27]]
// CHECK:   end_borrow [[REG29]]
// CHECK:   dealloc_stack [[REG4]]
// CHECK:   return [[REG32]]
// CHECK: }

// CHECK: sil [ossa] @$s25borrow_accessor_container17CopyableContainerVyAA5KlassCSiciz : $@convention(method) (Int, @inout CopyableContainer) -> @inout Klass {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*CopyableContainer):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], var, name "self", argno 2, expr op_deref
// CHECK:   [[REG4:%.*]] = alloc_stack $UnsafeMutablePointer<S>
// CHECK:   [[REG5:%.*]] = begin_access [read] [unknown] [[REG1]]
// CHECK:   [[REG6:%.*]] = struct_element_addr [[REG5]], #CopyableContainer._storage
// CHECK:   [[REG7:%.*]] = load [trivial] [[REG6]]
// CHECK:   end_access [[REG5]]
// CHECK:   [[REG9:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG10:%.*]] = apply [[REG9]]<S>([[REG7]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG11:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<S>>
// CHECK:   store [[REG10]] to [trivial] [[REG11]]
// CHECK:   [[REG13:%.*]] = alloc_stack $UnsafeMutablePointer<S>
// CHECK:   [[REG14:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG15:%.*]] = apply [[REG14]]<UnsafeMutablePointer<S>>([[REG13]], [[REG11]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG16:%.*]] = load [trivial] [[REG13]]
// CHECK:   [[REG17:%.*]] = alloc_stack $UnsafeMutablePointer<S>
// CHECK:   store [[REG16]] to [trivial] [[REG17]]
// CHECK:   [[REG19:%.*]] = function_ref @$ss8_PointerPsE8advanced2byxSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[REG20:%.*]] = apply [[REG19]]<UnsafeMutablePointer<S>>([[REG4]], [[REG0]], [[REG17]]) : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   dealloc_stack [[REG17]]
// CHECK:   dealloc_stack [[REG13]]
// CHECK:   dealloc_stack [[REG11]]
// CHECK:   [[REG24:%.*]] = load [trivial] [[REG4]]
// CHECK:   [[REG25:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvau : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG26:%.*]] = apply [[REG25]]<S>([[REG24]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG27:%.*]] = struct_extract [[REG26]], #UnsafeMutablePointer._rawValue
// CHECK:   [[REG28:%.*]] = pointer_to_address [[REG27]] to [strict] $*S
// CHECK:   [[REG29:%.*]] = mark_dependence [unresolved] [[REG28]] on [[REG24]]
// CHECK:   [[REG30:%.*]] = begin_access [modify] [unsafe] [[REG29]]
// CHECK:   [[REG31:%.*]] = struct_element_addr [[REG30]], #S._k
// CHECK:   end_access [[REG30]]
// CHECK:   dealloc_stack [[REG4]]
// CHECK:   return [[REG31]]
// CHECK: }

public struct NC : ~Copyable {}

public struct NonCopyableContainer : ~Copyable {
  var _storage: UnsafeMutableBufferPointer<NC>
  var _count: Int

  var first: NC {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.baseAddress.unsafelyUnwrapped.pointee
    }
  }

  public subscript(index: Int) -> NC {
    @_unsafeSelfDependentResult
    borrow {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return &_storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
  }
}

// CHECK: sil [ossa] @$s25borrow_accessor_container20NonCopyableContainerVyAA2NCVSicib : $@convention(method) (Int, @guaranteed NonCopyableContainer) -> @guaranteed NC {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : @guaranteed $NonCopyableContainer):
// CHECK:   [[REG3:%.*]] = copy_value [[REG1]]
// CHECK:   [[REG4:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG3]]
// CHECK:   debug_value [[REG4]], let, name "self", argno 2
// CHECK:   [[REG6:%.*]] = alloc_stack $UnsafeMutablePointer<NC>
// CHECK:   [[REG7:%.*]] = begin_borrow [[REG4]]
// CHECK:   [[REG8:%.*]] = struct_extract [[REG7]], #NonCopyableContainer._storage
// CHECK:   [[REG9:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG10:%.*]] = apply [[REG9]]<NC>([[REG8]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG11:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<NC>>
// CHECK:   store [[REG10]] to [trivial] [[REG11]]
// CHECK:   [[REG13:%.*]] = alloc_stack $UnsafeMutablePointer<NC>
// CHECK:   [[REG14:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG15:%.*]] = apply [[REG14]]<UnsafeMutablePointer<NC>>([[REG13]], [[REG11]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG16:%.*]] = load [trivial] [[REG13]]
// CHECK:   [[REG17:%.*]] = alloc_stack $UnsafeMutablePointer<NC>
// CHECK:   store [[REG16]] to [trivial] [[REG17]]
// CHECK:   [[REG19:%.*]] = function_ref @$ss8_PointerPsE8advanced2byxSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[REG20:%.*]] = apply [[REG19]]<UnsafeMutablePointer<NC>>([[REG6]], [[REG0]], [[REG17]]) : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   dealloc_stack [[REG17]]
// CHECK:   dealloc_stack [[REG13]]
// CHECK:   dealloc_stack [[REG11]]
// CHECK:   end_borrow [[REG7]]
// CHECK:   [[REG25:%.*]] = load [trivial] [[REG6]]
// CHECK:   [[REG26:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvlu : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG27:%.*]] = apply [[REG26]]<NC>([[REG25]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafePointer<τ_0_0>
// CHECK:   [[REG28:%.*]] = struct_extract [[REG27]], #UnsafePointer._rawValue
// CHECK:   [[REG29:%.*]] = pointer_to_address [[REG28]] to [strict] $*NC
// CHECK:   [[REG30:%.*]] = mark_dependence [unresolved] [[REG29]] on [[REG25]]
// CHECK:   [[REG31:%.*]] = begin_access [read] [unsafe] [[REG30]]
// CHECK:   [[REG32:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG31]]
// CHECK:   [[REG33:%.*]] = load_borrow [[REG32]]
// CHECK:   [[REG35:%.*]] = unchecked_ownership_conversion [[REG33]], @guaranteed to @unowned
// CHECK:   [[REG36:%.*]] = unchecked_ownership_conversion [[REG35]], @unowned to @guaranteed
// CHECK:   end_access [[REG31]]
// CHECK:   end_borrow [[REG33]]
// CHECK:   dealloc_stack [[REG6]]
// CHECK:   destroy_value [[REG4]]
// CHECK:   return [[REG36]]
// CHECK: }

// CHECK: sil [ossa] @$s25borrow_accessor_container20NonCopyableContainerVyAA2NCVSiciz : $@convention(method) (Int, @inout NonCopyableContainer) -> @inout NC {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*NonCopyableContainer):
// CHECK:   debug_value [[REG0]], let, name "index", argno 1
// CHECK:   debug_value [[REG1]], var, name "self", argno 2, expr op_deref
// CHECK:   [[REG4:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG1]]
// CHECK:   [[REG5:%.*]] = alloc_stack $UnsafeMutablePointer<NC>
// CHECK:   [[REG6:%.*]] = begin_access [read] [unknown] [[REG4]]
// CHECK:   [[REG7:%.*]] = struct_element_addr [[REG6]], #NonCopyableContainer._storage
// CHECK:   [[REG8:%.*]] = load [trivial] [[REG7]]
// CHECK:   end_access [[REG6]]
// CHECK:   [[REG10:%.*]] = function_ref @$sSr11baseAddressSpyxGSgvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG11:%.*]] = apply [[REG10]]<NC>([[REG8]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutableBufferPointer<τ_0_0>) -> Optional<UnsafeMutablePointer<τ_0_0>>
// CHECK:   [[REG12:%.*]] = alloc_stack $Optional<UnsafeMutablePointer<NC>>
// CHECK:   store [[REG11]] to [trivial] [[REG12]]
// CHECK:   [[REG14:%.*]] = alloc_stack $UnsafeMutablePointer<NC>
// CHECK:   [[REG15:%.*]] = function_ref @$sSq17unsafelyUnwrappedxvg : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG16:%.*]] = apply [[REG15]]<UnsafeMutablePointer<NC>>([[REG14]], [[REG12]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Escapable> (@in_guaranteed Optional<τ_0_0>) -> @lifetime(copy 0) @out τ_0_0
// CHECK:   [[REG17:%.*]] = load [trivial] [[REG14]]
// CHECK:   [[REG18:%.*]] = alloc_stack $UnsafeMutablePointer<NC>
// CHECK:   store [[REG17]] to [trivial] [[REG18]]
// CHECK:   [[REG20:%.*]] = function_ref @$ss8_PointerPsE8advanced2byxSi_tF : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[REG21:%.*]] = apply [[REG20]]<UnsafeMutablePointer<NC>>([[REG5]], [[REG0]], [[REG18]]) : $@convention(method) <τ_0_0 where τ_0_0 : _Pointer> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   dealloc_stack [[REG18]]
// CHECK:   dealloc_stack [[REG14]]
// CHECK:   dealloc_stack [[REG12]]
// CHECK:   [[REG25:%.*]] = load [trivial] [[REG5]]
// CHECK:   [[REG26:%.*]] = function_ref @$sSpsRi_zrlE7pointeexvau : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG27:%.*]] = apply [[REG26]]<NC>([[REG25]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (UnsafeMutablePointer<τ_0_0>) -> UnsafeMutablePointer<τ_0_0>
// CHECK:   [[REG28:%.*]] = struct_extract [[REG27]], #UnsafeMutablePointer._rawValue
// CHECK:   [[REG29:%.*]] = pointer_to_address [[REG28]] to [strict] $*NC
// CHECK:   [[REG30:%.*]] = mark_dependence [unresolved] [[REG29]] on [[REG25]]
// CHECK:   [[REG31:%.*]] = begin_access [modify] [unsafe] [[REG30]]
// CHECK:   [[REG32:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG31]]
// CHECK:   end_access [[REG31]]
// CHECK:   dealloc_stack [[REG5]]
// CHECK:   return [[REG32]]
// CHECK: }
