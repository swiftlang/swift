// RUN: %target-swift-frontend -c %s -Xllvm -sil-print-after=loadable-address  -enable-experimental-feature BorrowAndMutateAccessors  2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-irgen %s -enable-experimental-feature BorrowAndMutateAccessors  2>&1 | %FileCheck --check-prefixes=CHECK-IRGEN  %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: OS=macosx

public class Klass {
  var id: Int = 0
}

@inline(never)
func use(_ k: Klass) {
  print(k.id)
}

@inline(never)
func use(_ s: SmallStruct) {
  print(s.id)
}

@inline(never)
func use(_ tuple: (Klass, Klass, Klass, Klass, Klass, Klass, Klass, Klass)) {
  print(tuple.4.id)
}

@inline(never)
func use(_ ls: LargeProp) { 
  use(ls._largeTuple)
}

@inline(never)
func use(_ k: borrowing  NC) {
  print(k.id)
}

@inline(never)
func use(_ ls: borrowing LargeNCProp) { 
  use(ls.nc1)
  use(ls.nc2)
  use(ls.nc3)
  use(ls.nc4)
  use(ls.nc5)
  use(ls.nc6)
  use(ls.nc7)
  use(ls.nc8)
}

public struct LargeProp {
  var _largeTuple = (Klass(), Klass(), Klass(), Klass(), Klass(), Klass(), Klass(), Klass())
}

public struct LargeNCProp : ~Copyable {
  var nc1 = NC()
  var nc2 = NC()
  var nc3 = NC()
  var nc4 = NC()
  var nc5 = NC()
  var nc6 = NC()
  var nc7 = NC()
  var nc8 = NC()
}

public struct SmallStruct {
  var id = 0
}

public struct LargeStruct {
  var _k = Klass()
  var _t = (1, 2, 3, 4, 5, 6, 7, 8)
  var _largeTuple = (Klass(), Klass(), Klass(), Klass(), Klass(), Klass(), Klass(), Klass())
  var _largeProp = LargeProp()
  var _smallStruct = SmallStruct()

  var borrowKlass: Klass {
    borrow {
      return _k
    }
  }
  var borrowSmallStruct: SmallStruct {
    borrow {
      return _smallStruct
    }
  }
  var largePropBorrow: LargeProp {
    borrow {
      return _largeProp
    }
  }
  var largeTupleBorrow: (Klass, Klass, Klass, Klass, Klass, Klass, Klass, Klass) {
    borrow {
      return _largeTuple
    }
  }
  var nestedLargePropBorrow: LargeProp {
    borrow {
      return largePropBorrow
    }
  }
}

func test() {
  let l = LargeStruct()
  use(l.borrowKlass)
  use(l.borrowSmallStruct)
  let k = l.borrowKlass
  use(k)
  use(l.largeTupleBorrow)
  use(l.largePropBorrow)
  use(k)
}

public struct NC : ~Copyable {
  var id: Int = 0
}

public struct NCLargeStruct : ~Copyable {
  var _k = NC()
  var _largeProp = LargeNCProp()

  var borrowNC: NC {
    borrow {
      return _k
    }
  }
  var largePropBorrow: LargeNCProp {
    borrow {
      return _largeProp
    }
  }
  var nestedLargePropBorrow: LargeNCProp {
    borrow {
      return largePropBorrow
    }
  }
}

func nctest() {
  let l = NCLargeStruct()
  use(l.borrowNC)
  use(l.largePropBorrow)
}

// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV0A5KlassAA0F0Cvb : $@convention(method) (@in_guaranteed LargeStruct) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $*LargeStruct):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #LargeStruct._k
// CHECK:   [[REG3:%.*]] = load [[REG2]]
// CHECK:   return [[REG3]]
// CHECK: }

// IRGen result type is PtrTy because we are returning a class reference 
// CHECK-IRGEN: define hidden swiftcc ptr @"$s21borrow_accessor_large11LargeStructV0A5KlassAA0F0Cvb"(ptr noalias swiftself captures(none) dereferenceable(208) [[REG0:%.*]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._k = getelementptr inbounds nuw %T21borrow_accessor_large11LargeStructV, ptr [[REG0]], i32 0, i32 0
// CHECK-IRGEN:   [[REG1:%.*]] = load ptr, ptr %._k, align 8
// CHECK-IRGEN:   ret ptr [[REG1]]
// CHECK-IRGEN: }

// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV0a5SmallE0AA0fE0Vvb : $@convention(method) (@in_guaranteed LargeStruct) -> SmallStruct {
// CHECK: bb0([[REG0:%.*]] : $*LargeStruct):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #LargeStruct._smallStruct
// CHECK:   [[REG3:%.*]] = load [[REG2]]
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc i64 @"$s21borrow_accessor_large11LargeStructV0a5SmallE0AA0fE0Vvb"(ptr noalias swiftself captures(none) dereferenceable(208) [[REG0:%.*]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._smallStruct = getelementptr inbounds nuw %T21borrow_accessor_large11LargeStructV, ptr [[REG0]], i32 0, i32 4
// CHECK-IRGEN:   %._smallStruct.id = getelementptr inbounds nuw %T21borrow_accessor_large11SmallStructV, ptr %._smallStruct, i32 0, i32 0
// CHECK-IRGEN:   %._smallStruct.id._value = getelementptr inbounds nuw %TSi, ptr %._smallStruct.id, i32 0, i32 0
// CHECK-IRGEN:   [[REG1:%.*]] = load i64, ptr %._smallStruct.id._value, align 8
// CHECK-IRGEN:   ret i64 [[REG1]]
// CHECK-IRGEN: }

// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV0C10PropBorrowAA0dF0Vvb : $@convention(method) (@in_guaranteed LargeStruct) -> @guaranteed_address LargeProp {
// CHECK: bb0([[REG0:%.*]] : $*LargeStruct):
// CHECK: [[REG1:%.*]] = struct_element_addr [[REG0]], #LargeStruct._largeProp
// CHECK: return [[REG1]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc ptr @"$s21borrow_accessor_large11LargeStructV0C10PropBorrowAA0dF0Vvb"(ptr noalias swiftself dereferenceable(208) [[REG0:%.*]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._largeProp = getelementptr inbounds nuw %T21borrow_accessor_large11LargeStructV, ptr [[REG0]], i32 0, i32 3
// CHECK-IRGEN:   ret ptr %._largeProp
// CHECK-IRGEN: }

// LoadableByAddress does not transform large tuples
// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV0C11TupleBorrowAA5KlassC_A7Ftvb : $@convention(method) (@in_guaranteed LargeStruct) -> @guaranteed (Klass, Klass, Klass, Klass, Klass, Klass, Klass, Klass) {
// CHECK: bb0([[REG0:%.*]] : $*LargeStruct):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #LargeStruct._largeTuple
// CHECK:   [[REG3:%.*]] = load [[REG2]]
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc void @"$s21borrow_accessor_large11LargeStructV0C11TupleBorrowAA5KlassC_A7Ftvb"(ptr noalias sret(<{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>) captures(none) [[REG0:%.*]], ptr noalias swiftself captures(none) dereferenceable(208) [[REG1:%.*]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._largeTuple = getelementptr inbounds nuw %T21borrow_accessor_large11LargeStructV, ptr [[REG1]], i32 0, i32 2
// CHECK-IRGEN:   %._largeTuple.elt = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 0
// CHECK-IRGEN:   [[REG2:%.*]] = load ptr, ptr %._largeTuple.elt, align 8
// CHECK-IRGEN:   %._largeTuple.elt1 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 1
// CHECK-IRGEN:   [[REG3:%.*]] = load ptr, ptr %._largeTuple.elt1, align 8
// CHECK-IRGEN:   %._largeTuple.elt2 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 2
// CHECK-IRGEN:   [[REG4:%.*]] = load ptr, ptr %._largeTuple.elt2, align 8
// CHECK-IRGEN:   %._largeTuple.elt3 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 3
// CHECK-IRGEN:   [[REG5:%.*]] = load ptr, ptr %._largeTuple.elt3, align 8
// CHECK-IRGEN:   %._largeTuple.elt4 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 4
// CHECK-IRGEN:   [[REG6:%.*]] = load ptr, ptr %._largeTuple.elt4, align 8
// CHECK-IRGEN:   %._largeTuple.elt5 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 5
// CHECK-IRGEN:   [[REG7:%.*]] = load ptr, ptr %._largeTuple.elt5, align 8
// CHECK-IRGEN:   %._largeTuple.elt6 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 6
// CHECK-IRGEN:   [[REG8:%.*]] = load ptr, ptr %._largeTuple.elt6, align 8
// CHECK-IRGEN:   %._largeTuple.elt7 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 7
// CHECK-IRGEN:   [[REG9:%.*]] = load ptr, ptr %._largeTuple.elt7, align 8
// CHECK-IRGEN:   %.elt = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 0
// CHECK-IRGEN:   store ptr [[REG2]], ptr %.elt, align 8
// CHECK-IRGEN:   %.elt8 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 1
// CHECK-IRGEN:   store ptr [[REG3]], ptr %.elt8, align 8
// CHECK-IRGEN:   %.elt9 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 2
// CHECK-IRGEN:   store ptr [[REG4]], ptr %.elt9, align 8
// CHECK-IRGEN:   %.elt10 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 3
// CHECK-IRGEN:   store ptr [[REG5]], ptr %.elt10, align 8
// CHECK-IRGEN:   %.elt11 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 4
// CHECK-IRGEN:   store ptr [[REG6]], ptr %.elt11, align 8
// CHECK-IRGEN:   %.elt12 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 5
// CHECK-IRGEN:   store ptr [[REG7]], ptr %.elt12, align 8
// CHECK-IRGEN:   %.elt13 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 6
// CHECK-IRGEN:   store ptr [[REG8]], ptr %.elt13, align 8
// CHECK-IRGEN:   %.elt14 = getelementptr inbounds nuw <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 7
// CHECK-IRGEN:   store ptr [[REG9]], ptr %.elt14, align 8
// CHECK-IRGEN:   ret void
// CHECK-IRGEN: }

// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV06nestedD10PropBorrowAA0dG0Vvb : $@convention(method) (@in_guaranteed LargeStruct) -> @guaranteed_address LargeProp {
// CHECK: bb0([[REG0:%.*]] : $*LargeStruct):
// CHECK:   [[REG1:%.*]] = function_ref @$s21borrow_accessor_large11LargeStructV0C10PropBorrowAA0dF0Vvb : $@convention(method) (@in_guaranteed LargeStruct) -> @guaranteed_address LargeProp
// CHECK:   [[REG2:%.*]] = apply [[REG1]]([[REG0]]) : $@convention(method) (@in_guaranteed LargeStruct) -> @guaranteed_address LargeProp
// CHECK:   return [[REG2]]
// CHECK: } // end sil function '$s21borrow_accessor_large11LargeStructV06nestedD10PropBorrowAA0dG0Vvb'

// CHECK-IRGEN define hidden swiftcc ptr @"$s21borrow_accessor_large11LargeStructV06nestedD10PropBorrowAA0dG0Vvb"(ptr noalias swiftself dereferenceable(208) [[REG0:%.*]]) {{.*}} {
// CHECK-IRGEN entry:
// CHECK-IRGEN   [[REG1:%.*]] = call swiftcc ptr @"$s21borrow_accessor_large11LargeStructV0C10PropBorrowAA0dF0Vvb"(ptr noalias swiftself dereferenceable(208) [[REG0]])
// CHECK-IRGEN   ret ptr [[REG1]]
// CHECK-IRGEN }

// CHECK: sil hidden @$s21borrow_accessor_large13NCLargeStructV0A2NCAA0F0Vvb : $@convention(method) (@in_guaranteed NCLargeStruct) -> @guaranteed NC {
// CHECK: bb0([[REG0:%.*]] : $*NCLargeStruct):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #NCLargeStruct._k
// CHECK:   [[REG3:%.*]] = load [[REG2]]
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc i64 @"$s21borrow_accessor_large13NCLargeStructV0A2NCAA0F0Vvb"(ptr noalias swiftself captures(none) dereferenceable(72) [[REG0:%.*]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._k = getelementptr inbounds nuw %T21borrow_accessor_large13NCLargeStructV, ptr [[REG0]], i32 0, i32 0
// CHECK-IRGEN:   %._k.id = getelementptr inbounds nuw %T21borrow_accessor_large2NCV, ptr %._k, i32 0, i32 0
// CHECK-IRGEN:   %._k.id._value = getelementptr inbounds nuw %TSi, ptr %._k.id, i32 0, i32 0
// CHECK-IRGEN:   [[REG1:%.*]] = load i64, ptr %._k.id._value, align 8
// CHECK-IRGEN:   ret i64 [[REG1]]
// CHECK-IRGEN: }

// Note: ~Copyable types are returned indirectly via copy_addy which in turn generates a memcopy
// CHECK: sil hidden @$s21borrow_accessor_large13NCLargeStructV0C10PropBorrowAA11LargeNCPropVvb : $@convention(method) (@in_guaranteed NCLargeStruct) -> @guaranteed_address LargeNCProp {
// CHECK: bb0([[REG0:%.*]] : $*NCLargeStruct):
// CHECK:   [[REG1:%.*]] = struct_element_addr [[REG0]], #NCLargeStruct._largeProp
// CHECK:   return [[REG1]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc ptr @"$s21borrow_accessor_large13NCLargeStructV0C10PropBorrowAA11LargeNCPropVvb"(ptr noalias swiftself dereferenceable(72) [[REG1:%.*]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._largeProp = getelementptr inbounds nuw %T21borrow_accessor_large13NCLargeStructV, ptr [[REG1]], i32 0, i32 1
// CHECK-IRGEN:   ret ptr %._largeProp
// CHECK-IRGEN: }

// CHECK: sil hidden @$s21borrow_accessor_large13NCLargeStructV21nestedLargePropBorrowAA0G6NCPropVvb : $@convention(method) (@in_guaranteed NCLargeStruct) -> @guaranteed_address LargeNCProp {
// CHECK: bb0([[REG0:%.*]] : $*NCLargeStruct):
// CHECK:   [[REG1:%.*]] = function_ref @$s21borrow_accessor_large13NCLargeStructV0C10PropBorrowAA11LargeNCPropVvb : $@convention(method) (@in_guaranteed NCLargeStruct) -> @guaranteed_address LargeNCProp
// CHECK:   [[REG2:%.*]] = apply [[REG1]]([[REG0]]) : $@convention(method) (@in_guaranteed NCLargeStruct) -> @guaranteed_address LargeNCProp
// CHECK:   return [[REG2]]
// CHECK: } // end sil function '$s21borrow_accessor_large13NCLargeStructV21nestedLargePropBorrowAA0G6NCPropVvb'

// CHECK-IRGEN: define hidden swiftcc ptr @"$s21borrow_accessor_large13NCLargeStructV21nestedLargePropBorrowAA0G6NCPropVvb"(ptr noalias swiftself dereferenceable(72) [[REG0:%.*]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   [[REG1:%.*]] = call swiftcc ptr @"$s21borrow_accessor_large13NCLargeStructV0C10PropBorrowAA11LargeNCPropVvb"(ptr noalias swiftself dereferenceable(72) [[REG0]])
// CHECK-IRGEN:   ret ptr [[REG1]]
// CHECK-IRGEN: }
