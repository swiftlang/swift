// RUN: %swift -emit-sil -sdk=%S/Inputs %s | FileCheck %s

// FIXME: This shouldn't need to be a separate test when SILTypes actually use
// uncurried Swift types directly.

// CHECK: uncurried type: [thin] (x : Int64) -> Int64
// CHECK-NEXT: sil @_T10uncurrying3fooFT1xSi_Si
func foo(x:Int) -> Int {}

// CHECK: uncurried type: [thin] ((y : Int64), (x : Int64)) -> Int64
// CHECK-NEXT: sil @_T10uncurrying3barfT1xSi_FT1ySi_Si
func bar(x:Int)(y:Int) -> Int {}

// CHECK: uncurried type: [thin] <T, U> ((y : U), (x : T)) -> Int64
// CHECK-NEXT: sil @_T10uncurrying3zimU___fT1xQ__FT1yQ0__Si
func zim<T, U>(x:T)(y:U) -> Int {}

// CHECK: uncurried type: [thin] ((z : Int64), (y : Int64), (x : Int64)) -> Int64
// CHECK-NEXT: sil @_T10uncurrying4zangfT1xSi_fT1ySi_FT1zSi_Si
func zang(x:Int)(y:Int)(z:Int) -> Int {}

struct Hoozit {
  // CHECK: uncurried type: [cc(method), thin] ((x : Int64), [byref] Hoozit) -> ()
  // CHECK-NEXT: sil @_TV10uncurrying6Hoozit3foofRS0_FT1xSi_T_
  func foo(x:Int) { }

  // CHECK: uncurried type: [cc(method), thin] <U> ((x : U), [byref] Hoozit) -> ()
  // CHECK-NEXT: sil @_TV10uncurrying6Hoozit3barfRS0_U__FT1xQ__T_
  func bar<U>(x:U) { }
}

struct Wotsit<T> {
  struct Thingamabob {
    // CHECK: uncurried type: [cc(method), thin] <T> ((x : T), [byref] Wotsit<T>.Thingamabob) -> ()
    // CHECK-NEXT: sil @_TVV10uncurrying6Wotsit11Thingamabob3fooU__fRS1_FT1xQ__T_
    func foo(x:T) { }

    // CHECK: uncurried type: [cc(method), thin] <T, U> ((x : U), [byref] Wotsit<T>.Thingamabob) -> ()
    // CHECK-NEXT: sil @_TVV10uncurrying6Wotsit11Thingamabob3barU__fRS1_U__FT1xQ__T_
    func bar<U>(x:U) { }
  }
}

// ObjC method types uncurry left-to-right.
class [objc] Knicknack {
  // CHECK: uncurried type: [cc(objc_method), thin] (Knicknack, (x : Int64)) -> ()
  // CHECK-NEXT: sil @_TToCSo9Knicknack3foofS_FT1xSi_T_
  func foo(x:Int) { }
}
