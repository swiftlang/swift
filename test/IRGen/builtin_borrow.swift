// RUN: %target-swift-frontend -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature BuiltinModule -enable-experimental-feature Lifetimes -emit-ir %s | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_Lifetimes

import Builtin

struct BorrowSmall: ~Escapable {
       let b: Builtin.Borrow<AnyObject>

       @_lifetime(borrow value)
       init(borrowing value: borrowing AnyObject) {
               self.b = Builtin.makeBorrow(value)
       }

       var value: AnyObject {
               borrow {
                       return Builtin.dereferenceBorrow(b)
               }
       }
}

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

struct BorrowAO: ~Escapable {
       let b: Builtin.Borrow<Any>

       @_lifetime(borrow value)
       init(borrowing value: borrowing Any) {
               self.b = Builtin.makeBorrow(value)
       }

       var value: Any {
               borrow {
                       return Builtin.dereferenceBorrow(b)
               }
       }
}

// TODO: addressable-for-dependencies, layout-dependent
