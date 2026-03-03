// RUN: %target-swift-frontend -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature BuiltinModule -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes -emit-ir %s | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

import Builtin

struct BorrowSmall: ~Escapable {
  let b: Builtin.Borrow<AnyObject>

  // referent is received by value as %0. borrow is a bitwise copy
  // of the representation in %0, so we return %0 directly
  // CHECK-LABEL: define{{.*}} @"{{.*}}11BorrowSmallV9borrowing
  // CHECK:   ret ptr %0
  @_lifetime(borrow value)
  init(borrowing value: borrowing AnyObject) {
    self.b = Builtin.makeBorrow(value)
  }

  var value: AnyObject {
    // CHECK-LABEL: define{{.*}} @"{{.*}}11BorrowSmallV5value{{.*}}b
    // CHECK:   ret ptr %0
    borrow {
      return Builtin.dereferenceBorrow(b)
    }
  }
}

/* TODO. waiting on rdar://167713693
struct Big { var a, b, c, d, e: AnyObject }

struct BorrowBig: ~Escapable {
  let b: Builtin.Borrow<Big>

  @_lifetime(borrow value)
  init(borrowing value: borrowing Big) {
    self.b = Builtin.makeBorrow(value)
  }

  var value: Big {
    borrow {
      return Builtin.dereferenceBorrow(b)
    }
  }
}
*/

struct BorrowAO: ~Escapable {
  let b: Builtin.Borrow<Any>

  // referent is received by address as %0. borrow is a pointer
  // to the value, so we return %0 directly
  // CHECK-LABEL: define{{.*}} @"{{.*}}8BorrowAOV9borrowing
  // CHECK:   ret ptr %0
  @_lifetime(borrow value)
  init(borrowing value: borrowing Any) {
    self.b = Builtin.makeBorrow(value)
  }

  var value: Any {
    // CHECK-LABEL: define{{.*}} @"{{.*}}8BorrowAOV5value{{.*}}b
    // CHECK:   ret ptr %0
    borrow {
      return Builtin.dereferenceBorrow(b)
    }
  }
}

@_addressableForDependencies
struct AFD {
  var x: Int
}

struct BorrowAFD: ~Escapable {
  let b: Builtin.Borrow<AFD>

  // referent is received by address as %0. borrow is a pointer
  // to the value, so we return %0 directly
  // CHECK-LABEL: define{{.*}} @"{{.*}}9BorrowAFDV9borrowing
  // CHECK:   ret ptr %0
  @_lifetime(borrow value)
  init(borrowing value: borrowing AFD) {
    self.b = Builtin.makeBorrow(value)
  }

  var value: AFD {
    // CHECK-LABEL: define{{.*}} @"{{.*}}9BorrowAFDV5value{{.*}}b
    // CHECK:   ret ptr %0
    borrow {
      return Builtin.dereferenceBorrow(b)
    }
  }
  
}

// TODO: addressable-for-dependencies, layout-dependent
