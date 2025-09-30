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
  }

  public subscript(index: Int) -> Element {
    @_unsafeSelfDependentResult
    borrow {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
  }
}

extension Container: Copyable where Element: Copyable {}


// CHECK: sil [ossa] @$s25borrow_accessor_container9ContainerVAARi_zrlEyxSicib : $@convention(method) <Element where Element : ~Copyable> (Int, @guaranteed Container<Element>) -> @guaranteed_addr Element {
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
 
