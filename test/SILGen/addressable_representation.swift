// RUN: %target-swift-emit-silgen -enable-experimental-feature Lifetimes -enable-experimental-feature BuiltinModule -enable-experimental-feature AddressableTypes -enable-experimental-feature AddressableParameters -module-name main %s -define-availability 'Span 0.1:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999' | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_AddressableParameters

import Builtin

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test1
@available(Span 0.1, *)
func test1(_ s: String) {
  // CHECK: [[ADDRESSABLE:%.*]] = alloc_stack $CO1
  // CHECK: [[ADDRESSABLE_BORROW:%.*]] = store_borrow {{.*}} to [[ADDRESSABLE]]
  let c = CO1(s)
  // CHECK: function_ref @$s{{.*}}10getStorage
  let storage = c.getStorage()
  // CHECK: function_ref @$s{{.*}}4utf8{{.*}}vg :
  let v = Array(storage[0].utf8)
  // CHECK: function_ref @$s{{.*}}5print
  print(v)
  // CHECK: end_borrow [[ADDRESSABLE_BORROW]]
  // CHECK: dealloc_stack [[ADDRESSABLE]]
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test2
@available(Span 0.1, *)
func test2(_ s: String, _ c1: Bool, _ c2: Bool) {
  // CHECK: [[ADDRESSABLE:%.*]] = alloc_stack $CO1
  // CHECK: [[ADDRESSABLE_BORROW:%.*]] = store_borrow {{.*}} to [[ADDRESSABLE]]
  let c = CO1(s)

  // CHECK:   cond_br {{.*}}, [[EXIT1:bb[0-9]+]], [[CONT1:bb[0-9]+]]
  // CHECK: [[EXIT1]]:
  // CHECK:   end_borrow [[ADDRESSABLE_BORROW]]
  // CHECK:   dealloc_stack [[ADDRESSABLE]]
  // CHECK:   br [[EPILOG:bb[0-9]+]]
  // CHECK: [[CONT1]]:
  if c1 {
      return
  }

  let storage = c.getStorage()

  // CHECK:   cond_br {{.*}}, [[EXIT2:bb[0-9]+]], [[CONT2:bb[0-9]+]]
  // CHECK: [[EXIT2]]:
  // CHECK:   end_borrow [[ADDRESSABLE_BORROW]]
  // CHECK:   dealloc_stack [[ADDRESSABLE]]
  // CHECK:   br [[EPILOG]]
  // CHECK: [[CONT2]]:
  if c2 {
      return
  }

  let v = Array(storage[0].utf8)
  print(v)
  // CHECK:   function_ref @$s{{.*}}5print
  // CHECK:   end_borrow [[ADDRESSABLE_BORROW]]
  // CHECK:   dealloc_stack [[ADDRESSABLE]]
  // CHECK:   br [[EPILOG]]

}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test3
@available(Span 0.1, *)
func test3(_ c: CO1) {
  // CHECK: [[ENTRY:bb[0-9]+]]([[C:%[0-9]+]] : @guaranteed $CO1):
  // CHECK:   [[ADDRESSABLE:%.*]] = alloc_stack $CO1
  // CHECK:   [[ADDRESSABLE_BORROW:%.*]] = store_borrow [[C]] to [[ADDRESSABLE]]
  // CHECK: function_ref @$s{{.*}}10getStorage
  let storage = c.getStorage()
  // CHECK: function_ref @$s{{.*}}4utf8{{.*}}vg :
  let v = Array(storage[0].utf8)
  // CHECK: function_ref @$s{{.*}}5print
  print(v)
  // CHECK: end_borrow [[ADDRESSABLE_BORROW]]
  // CHECK: dealloc_stack [[ADDRESSABLE]]
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}6test3a
@available(Span 0.1, *)
func test3a(_ c: borrowing CO1) {
  // CHECK: [[ENTRY:bb[0-9]+]]([[C:%[0-9]+]] : @noImplicitCopy @guaranteed $CO1):
  // CHECK:   [[WRAP:%.*]] = copyable_to_moveonlywrapper [guaranteed] [[C]]
  // CHECK:   [[COPY:%.*]] = copy_value [[WRAP]]
  // CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
  // CHECK:   [[ADDRESSABLE:%.*]] = alloc_stack $@moveOnly CO1
  // CHECK:   [[ADDRESSABLE_BORROW:%.*]] = store_borrow [[MARK]] to [[ADDRESSABLE]]
  // CHECK: function_ref @$s{{.*}}10getStorage
  let storage = c.getStorage()
  // CHECK: function_ref @$s{{.*}}4utf8{{.*}}vg :
  let v = Array(storage[0].utf8)
  // CHECK: function_ref @$s{{.*}}5print
  print(v)
  // CHECK: end_borrow [[ADDRESSABLE_BORROW]]
  // CHECK: dealloc_stack [[ADDRESSABLE]]
  // CHECK: destroy_value [[MARK]]
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test4
@available(Span 0.1, *)
func test4(_ c: CO1, _ c1: Bool, _ c2: Bool) {
  // CHECK: [[ENTRY:bb[0-9]+]]([[C:%[0-9]+]] : @guaranteed $CO1, {{.*}}):
  // CHECK:   [[ADDRESSABLE:%.*]] = alloc_stack $CO1
  // CHECK:   [[ADDRESSABLE_BORROW:%.*]] = store_borrow [[C]] to [[ADDRESSABLE]]

  // CHECK:   cond_br {{.*}}, [[EXIT1:bb[0-9]+]], [[CONT1:bb[0-9]+]]
  // CHECK: [[EXIT1]]:
  // CHECK:   br [[EPILOG:bb[0-9]+]]
  // CHECK: [[CONT1]]:
  if c1 {
      return
  }

  let storage = c.getStorage()

  // CHECK:   cond_br {{.*}}, [[EXIT2:bb[0-9]+]], [[CONT2:bb[0-9]+]]
  // CHECK: [[EXIT2]]:
  // CHECK:   br [[EPILOG]]
  // CHECK: [[CONT2]]:
  if c2 {
      return
  }

  let v = Array(storage[0].utf8)
  print(v)
  // CHECK:   function_ref @$s{{.*}}5print
  // CHECK:   br [[EPILOG]]
  
  // CHECK: [[EPILOG]]:
  // CHECK:   end_borrow [[ADDRESSABLE_BORROW]]
  // CHECK:   dealloc_stack [[ADDRESSABLE]]
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test5
func test5(_ f: @escaping () -> ()) {
    // CHECK: [[ADDRESSABLE:%.*]] = alloc_stack
    // CHECK: [[THUNKED:%.*]] = partial_apply
    // CHECK: [[CONV:%.*]] = convert_function [[THUNKED]]
    // CHECK: [[ADDRESSABLE_BORROW:%.*]] = store_borrow [[CONV]] to [[ADDRESSABLE]]

    // CHECK: function_ref @$s{{.*}}22addressableFunctionArg
    addressableFunctionArg(f)
    // CHECK: function_ref @$s{{.*}}22addressableFunctionArg
    addressableFunctionArg(f)

    // CHECK: end_borrow [[ADDRESSABLE_BORROW]]
    // CHECK: dealloc_stack [[ADDRESSABLE]]
    // CHECK: destroy_value [[CONV]]
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test6
@available(Span 0.1, *)
func test6(_ c: inout CO1) {
  // CHECK: [[ENTRY:bb[0-9]+]]([[C:%[0-9]+]] : $*CO1):

  // SILGen still only emits an access around the immediate call, but
  // lifetime analysis can extend this access scope.

  // CHECK:   [[ACCESS:%.*]] = begin_access [read] [static] [[C]]
  // CHECK:   function_ref @$s{{.*}}10getStorage
  // CHECK:   end_access [[ACCESS]]
  let storage = c.getStorage()
  let v = Array(storage[0].utf8)
  print(v)
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test7
@available(Span 0.1, *)
func test7(_ c: CO1) {
  var c2 = c

  // SILGen still only emits an access around the immediate call, but
  // lifetime analysis can extend this access scope.

  // CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic]
  // CHECK:   function_ref @$s{{.*}}10getStorage
  // CHECK:   end_access [[ACCESS]]
  let storage = c2.getStorage()
  let v = Array(storage[0].utf8)
  print(v)
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}5test8
func test8() {
  guard #available(Span 0.1, *) else { return }

  var s = "A long string that is absolutely not smol at all."
  let u = Array(s.utf8)

  // CHECK: [[C_ADDRESSABLE:%.*]] = alloc_stack $CO2<String>
  // CHECK: [[C_ADDRESSABLE_BORROW:%.*]] = store_borrow {{%.*}} to [[C_ADDRESSABLE]]
  let c = CO2(consume s)
  s = ""
  // CHECK: [[GET_STORAGE:%.*]] = function_ref @$s{{.*}}7storage{{.*}}Gvg
  // CHECK: apply [[GET_STORAGE]]<String>([[C_ADDRESSABLE_BORROW]]
  let span = c.storage

  _ = span.count == 1

  // CHECK: function_ref @$s{{.*}}4utf8
  let v = Array(span[0].utf8)
  _ = u == v
  // CHECK: end_borrow [[C_ADDRESSABLE_BORROW]]
}


func addressableFunctionArg(_ f: @_addressable @escaping () -> ()) {}

@available(Span 0.1, *)
@_addressableForDependencies
struct CO1 {
    var s: String

    init(_ s: String) { self.s = s }

    var storage: Span<String> {
        @_lifetime(borrow self)
        borrowing get {
            fatalError()
        }
    }

    @_lifetime(borrow self)
    func getStorage() -> Span<String> {
        fatalError()
    }
}

@available(Span 0.1, *)
@_addressableForDependencies
struct CO2<T> {
    var s: T

    init(_ s: T) { self.s = s }

    var storage: Span<T> {
        @_lifetime(borrow self)
        borrowing get {
            fatalError()
        }
    }

    @_lifetime(borrow self)
    func getStorage() -> Span<T> {
        fatalError()
    }
}

