// RUN: %target-swift-emit-silgen -module-name main \
// RUN: -enable-experimental-feature BuiltinModule \
// RUN: -enable-experimental-feature AddressableTypes \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: %s | %FileCheck %s

// RUN: %target-swift-emit-sil -module-name main \
// RUN: -enable-experimental-feature BuiltinModule \
// RUN: -enable-experimental-feature AddressableTypes \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: %s | %FileCheck --check-prefix=CHECK-POST-CLEANUP %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_Lifetimes

import Builtin

struct NonGeneric {
  var x: AnyObject
}

struct NonGenericAO {
  var x: Any
}

struct Fixed<T> {
  var x: Int
}

@_addressableForDependencies
struct AFD { var x: AnyObject }

@_addressableForDependencies
struct AFDTrivial { var x: Int }

struct BorrowTrivial: ~Escapable {
  var pointer: Builtin.RawPointer

  var value: Int {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}13BorrowTrivialV5value{{.*}}vb :
    // CHECK:       bb0([[STRUCT:%.*]] :
    // CHECK:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK:         [[ADDR:%.*]] = pointer_to_address [[POINTER]]
    // CHECK:         [[REFERENT:%.*]] = load [trivial] [[ADDR]]
    // CHECK:         return [[REFERENT]]
    @_unsafeSelfDependentResult
    borrow {
      return Builtin.borrowAt(pointer)
    }
  }
}

struct BorrowLoadable: ~Escapable {
  var pointer: Builtin.RawPointer

  var value: NonGeneric {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}14BorrowLoadableV5value{{.*}}vb :
    // CHECK:       bb0([[STRUCT:%.*]] :
    // CHECK:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK:         [[ADDR:%.*]] = pointer_to_address [[POINTER]]
    // CHECK:         [[REFERENT:%.*]] = load_borrow [[ADDR]]
    // CHECK:         [[UNCHECKED:%.*]] = unchecked_ownership [[REFERENT]]
    // CHECK:         end_borrow [[REFERENT]]
    // CHECK:         return [[UNCHECKED]]
    //
    // SILGenCleanup fixes this to the more principled `return_borrow`,
    // which after ownership elimination gives us:
    // CHECK-POST-CLEANUP-LABEL: sil{{.*}} @$s{{.*}}14BorrowLoadableV5value{{.*}}vb :
    // CHECK-POST-CLEANUP:       bb0([[STRUCT:%.*]] :
    // CHECK-POST-CLEANUP:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK-POST-CLEANUP:         [[ADDR:%.*]] = pointer_to_address [[POINTER]]
    // CHECK-POST-CLEANUP:         [[REFERENT:%.*]] = load [[ADDR]]
    // CHECK-POST-CLEANUP:         return [[REFERENT]]
    @_unsafeSelfDependentResult
    borrow {
      return Builtin.borrowAt(pointer)
    }
  }
}

struct BorrowAO: ~Escapable {
  var pointer: Builtin.RawPointer

  var value: NonGenericAO {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}8BorrowAOV5value{{.*}}vb :
    // CHECK:       bb0([[STRUCT:%.*]] :
    // CHECK:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK:         [[REFERENT:%.*]] = pointer_to_address [[POINTER]]
    // CHECK-NOT:     load
    // CHECK:         return [[REFERENT]]
    @_unsafeSelfDependentResult
    borrow {
      return Builtin.borrowAt(pointer)
    }
  }
}

struct BorrowAFD: ~Escapable {
  var pointer: Builtin.RawPointer

  var value: AFD {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}9BorrowAFDV5value{{.*}}vb :
    // CHECK:       bb0([[STRUCT:%.*]] :
    // CHECK:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK:         [[REFERENT:%.*]] = pointer_to_address [[POINTER]]
    // CHECK:         load_borrow [[REFERENT]]
    // CHECK:         return [[REFERENT]]

    // CHECK-POST-CLEANUP-LABEL: sil{{.*}} @$s{{.*}}9BorrowAFDV5value{{.*}}vb :
    // CHECK-POST-CLEANUP:       bb0([[STRUCT:%.*]] :
    // CHECK-POST-CLEANUP:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK-POST-CLEANUP:         [[REFERENT:%.*]] = pointer_to_address [[POINTER]]
    // CHECK-POST-CLEANUP-NOT:     load
    // CHECK-POST-CLEANUP:         return [[REFERENT]]
    @_unsafeSelfDependentResult
    borrow {
      return Builtin.borrowAt(pointer)
    }
  }
}

struct BorrowAFDTrivial: ~Escapable {
  var pointer: Builtin.RawPointer

  var value: AFDTrivial {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}16BorrowAFDTrivialV5value{{.*}}vb :
    // CHECK:       bb0([[STRUCT:%.*]] :
    // CHECK:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK:         [[REFERENT:%.*]] = pointer_to_address [[POINTER]]
    // CHECK:         load [trivial] [[REFERENT]]
    // CHECK:         return [[REFERENT]]

    // CHECK-POST-CLEANUP-LABEL: sil{{.*}} @$s{{.*}}16BorrowAFDTrivialV5value{{.*}}vb :
    // CHECK-POST-CLEANUP:       bb0([[STRUCT:%.*]] :
    // CHECK-POST-CLEANUP:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK-POST-CLEANUP:         [[REFERENT:%.*]] = pointer_to_address [[POINTER]]
    // CHECK-POST-CLEANUP-NOT:     load
    // CHECK-POST-CLEANUP:         return [[REFERENT]]
    @_unsafeSelfDependentResult
    borrow {
      return Builtin.borrowAt(pointer)
    }
  }
}

struct BorrowDep<T>: ~Escapable {
  private var pointer: Builtin.RawPointer

  var value: T {
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}9BorrowDepV5value{{.*}}vb :
    // CHECK:       bb0([[STRUCT:%.*]] :
    // CHECK:         [[POINTER:%.*]] = struct_extract [[STRUCT]]
    // CHECK:         [[REFERENT:%.*]] = pointer_to_address [[POINTER]]
    // CHECK-NOT:     load
    // CHECK:         return [[REFERENT]]
    @_unsafeSelfDependentResult
    borrow {
      return Builtin.borrowAt(pointer)
    }
  }
}
