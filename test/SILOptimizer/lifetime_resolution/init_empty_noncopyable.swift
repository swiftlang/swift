// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -sil-verify-all -module-name init_empty_noncopyable \
// RUN:  -sil-stop-optzns-before-lowering-ownership -enable-experimental-feature RawLayout %s | %FileCheck %s --enable-var-scope

// REQUIRES: swift_feature_RawLayout

// SILGen initializes `self` of an empty noncopyable struct itself, so it starts out uninitialized.

struct Empty: ~Copyable {
  deinit {}
}

func makeEmpty() -> Empty { Empty() }

// CHECK-LABEL: sil {{.*}}@$s22init_empty_noncopyable5EmptyVACycfC :
// CHECK:         [[SELF:%[0-9]+]] = alloc_stack {{.*}}"self"
// CHECK:         [[VAL:%[0-9]+]] = struct $Empty ()
// CHECK-NEXT:    store [[VAL]] to [init] [[SELF]]
// CHECK-NEXT:    load [take] [[SELF]]
// CHECK-LABEL: } // end sil function '$s22init_empty_noncopyable5EmptyVACycfC'

struct Reassigned: ~Copyable {
  init() {
    self = Reassigned(0)
  }
  init(_ x: Int) {}
  deinit {}
}

// CHECK-LABEL: sil {{.*}}@$s22init_empty_noncopyable10ReassignedVACycfC :
// CHECK:         [[SELF:%[0-9]+]] = alloc_stack {{.*}}"self"
// CHECK:         [[VAL:%[0-9]+]] = struct $Reassigned ()
// CHECK-NEXT:    store [[VAL]] to [init] [[SELF]]
// CHECK:         apply
// CHECK:         [[ACCESS:%[0-9]+]] = begin_access [modify] [static] [[SELF]]
// CHECK:         store {{%[0-9]+}} to [assign] [[ACCESS]]
// CHECK:         load [take] [[SELF]]
// CHECK:         dealloc_stack [[SELF]]
// CHECK-LABEL: } // end sil function '$s22init_empty_noncopyable10ReassignedVACycfC'

@_rawLayout(like: T)
struct Cell<T>: ~Copyable {
  init() {}
  deinit {}
}

// CHECK-LABEL: sil {{.*}}@$s22init_empty_noncopyable4CellVACyxGycfC :
// CHECK:         [[SELF:%[0-9]+]] = alloc_stack {{.*}}"self"
// CHECK-NEXT:    builtin "prepareInitialization"([[SELF]])
// CHECK-NEXT:    copy_addr [take] [[SELF]] to [init]
// CHECK-NOT:     destroy_addr
// CHECK-LABEL: } // end sil function '$s22init_empty_noncopyable4CellVACyxGycfC'

// A copyable empty struct is trivial: SILGen emits no initialization of `self`.
struct EmptyCopyable {
  init() {}
}

func makeEmptyCopyable() -> EmptyCopyable { EmptyCopyable() }
