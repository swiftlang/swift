// RUN: %target-swift-frontend -c %s -Xllvm -sil-print-after=loadable-address  -enable-experimental-feature BorrowAndMutateAccessors  2>&1 | %FileCheck %s

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
// CHECK-IRGEN: define hidden swiftcc ptr @"$s21borrow_accessor_large11LargeStructV0A5KlassAA0F0Cvb"(ptr noalias nocapture swiftself dereferenceable(208) [[REG0]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._k = getelementptr inbounds %T21borrow_accessor_large11LargeStructV, ptr [[REG0]], i32 0, i32 0
// CHECK-IRGEN:   [[REG1:%.*]] = load ptr, ptr %._k, align 8
// CHECK-IRGEN:   ret ptr [[REG1]]
// CHECK-IRGEN: }

// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV0a5SmallE0AA0fE0Vvb : $@convention(method) (@in_guaranteed LargeStruct) -> SmallStruct {
// CHECK: bb0([[REG0:%.*]] : $*LargeStruct):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #LargeStruct._smallStruct
// CHECK:   [[REG3:%.*]] = load [[REG2]]
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc i64 @"$s21borrow_accessor_large11LargeStructV0a5SmallE0AA0fE0Vvb"(ptr noalias nocapture swiftself dereferenceable(208) [[REG0]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._smallStruct = getelementptr inbounds %T21borrow_accessor_large11LargeStructV, ptr [[REG0]], i32 0, i32 4
// CHECK-IRGEN:   %._smallStruct.id = getelementptr inbounds %T21borrow_accessor_large11SmallStructV, ptr %._smallStruct, i32 0, i32 0
// CHECK-IRGEN:   %._smallStruct.id._value = getelementptr inbounds %TSi, ptr %._smallStruct.id, i32 0, i32 0
// CHECK-IRGEN:   [[REG1:%.*]] = load i64, ptr %._smallStruct.id._value, align 8
// CHECK-IRGEN:   ret i64 [[REG1]]
// CHECK-IRGEN: }

// LoadableByAddress transforms to an indirect result
// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV0C10PropBorrowAA0dF0Vvb : $@convention(method) (@in_guaranteed LargeStruct) -> @out LargeProp {
// CHECK: bb0([[REG0:%.*]] : $*LargeProp, [[REG1:%.*]] : $*LargeStruct):
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG1]], #LargeStruct._largeProp
// CHECK:   copy_addr [take] [[REG3]] to [init] [[REG0]]
// CHECK:   [[REG5:%.*]] = tuple ()
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc void @"$s21borrow_accessor_large11LargeStructV0C10PropBorrowAA0dF0Vvb"(ptr noalias nocapture sret(%T21borrow_accessor_large9LargePropV) [[REG0]], ptr noalias nocapture swiftself dereferenceable(208) [[REG1]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._largeProp = getelementptr inbounds %T21borrow_accessor_large11LargeStructV, ptr [[REG1]], i32 0, i32 3
// CHECK-IRGEN:   call void @llvm.memcpy.p0.p0.i64(ptr align 8 [[REG0]], ptr align 8 %._largeProp, i64 64, i1 false)
// CHECK-IRGEN:   ret void
// CHECK-IRGEN: }

// LoadableByAddress does not transform large tuples
// CHECK: sil hidden @$s21borrow_accessor_large11LargeStructV0C11TupleBorrowAA5KlassC_A7Ftvb : $@convention(method) (@in_guaranteed LargeStruct) -> @guaranteed (Klass, Klass, Klass, Klass, Klass, Klass, Klass, Klass) {
// CHECK: bb0([[REG0:%.*]] : $*LargeStruct):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #LargeStruct._largeTuple
// CHECK:   [[REG3:%.*]] = load [[REG2]]
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc void @"$s21borrow_accessor_large11LargeStructV0C11TupleBorrowAA5KlassC_A7Ftvb"(ptr noalias nocapture sret(<{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>) [[REG0]], ptr noalias nocapture swiftself dereferenceable(208) [[REG1]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._largeTuple = getelementptr inbounds %T21borrow_accessor_large11LargeStructV, ptr [[REG1]], i32 0, i32 2
// CHECK-IRGEN:   %._largeTuple.elt = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 0
// CHECK-IRGEN:   [[REG2:%.*]] = load ptr, ptr %._largeTuple.elt, align 8
// CHECK-IRGEN:   %._largeTuple.elt1 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 1
// CHECK-IRGEN:   [[REG3:%.*]] = load ptr, ptr %._largeTuple.elt1, align 8
// CHECK-IRGEN:   %._largeTuple.elt2 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 2
// CHECK-IRGEN:   [[REG4:%.*]] = load ptr, ptr %._largeTuple.elt2, align 8
// CHECK-IRGEN:   %._largeTuple.elt3 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 3
// CHECK-IRGEN:   [[REG5:%.*]] = load ptr, ptr %._largeTuple.elt3, align 8
// CHECK-IRGEN:   %._largeTuple.elt4 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 4
// CHECK-IRGEN:   [[REG6:%.*]] = load ptr, ptr %._largeTuple.elt4, align 8
// CHECK-IRGEN:   %._largeTuple.elt5 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 5
// CHECK-IRGEN:   [[REG7:%.*]] = load ptr, ptr %._largeTuple.elt5, align 8
// CHECK-IRGEN:   %._largeTuple.elt6 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 6
// CHECK-IRGEN:   [[REG8:%.*]] = load ptr, ptr %._largeTuple.elt6, align 8
// CHECK-IRGEN:   %._largeTuple.elt7 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr %._largeTuple, i32 0, i32 7
// CHECK-IRGEN:   [[REG9:%.*]] = load ptr, ptr %._largeTuple.elt7, align 8
// CHECK-IRGEN:   %.elt = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 0
// CHECK-IRGEN:   store ptr [[REG2]], ptr %.elt, align 8
// CHECK-IRGEN:   %.elt8 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 1
// CHECK-IRGEN:   store ptr [[REG3]], ptr %.elt8, align 8
// CHECK-IRGEN:   %.elt9 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 2
// CHECK-IRGEN:   store ptr [[REG4]], ptr %.elt9, align 8
// CHECK-IRGEN:   %.elt10 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 3
// CHECK-IRGEN:   store ptr [[REG5]], ptr %.elt10, align 8
// CHECK-IRGEN:   %.elt11 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 4
// CHECK-IRGEN:   store ptr [[REG6]], ptr %.elt11, align 8
// CHECK-IRGEN:   %.elt12 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 5
// CHECK-IRGEN:   store ptr [[REG7]], ptr %.elt12, align 8
// CHECK-IRGEN:   %.elt13 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 6
// CHECK-IRGEN:   store ptr [[REG8]], ptr %.elt13, align 8
// CHECK-IRGEN:   %.elt14 = getelementptr inbounds <{ ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr }>, ptr [[REG0]], i32 0, i32 7
// CHECK-IRGEN:   store ptr [[REG9]], ptr %.elt14, align 8
// CHECK-IRGEN:   ret void
// CHECK-IRGEN: }

// CHECK: sil hidden @$s21borrow_accessor_large13NCLargeStructV0A2NCAA0F0Vvb : $@convention(method) (@in_guaranteed NCLargeStruct) -> @guaranteed NC {
// CHECK: bb0([[REG0:%.*]] : $*NCLargeStruct):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #NCLargeStruct._k
// CHECK:   [[REG3:%.*]] = load [[REG2]]
// CHECK:   return [[REG3]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc i64 @"$s21borrow_accessor_large13NCLargeStructV0A2NCAA0F0Vvb"(ptr noalias nocapture swiftself dereferenceable(72) [[REG0]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._k = getelementptr inbounds %T21borrow_accessor_large13NCLargeStructV, ptr [[REG0]], i32 0, i32 0
// CHECK-IRGEN:   %._k.id = getelementptr inbounds %T21borrow_accessor_large2NCV, ptr %._k, i32 0, i32 0
// CHECK-IRGEN:   %._k.id._value = getelementptr inbounds %TSi, ptr %._k.id, i32 0, i32 0
// CHECK-IRGEN:   [[REG1:%.*]] = load i64, ptr %._k.id._value, align 8
// CHECK-IRGEN:   ret i64 [[REG1]]
// CHECK-IRGEN: }

// Note: ~Copyable types are returned indirectly via copy_addy which in turn generates a memcopy
// CHECK: sil hidden @$s21borrow_accessor_large13NCLargeStructV0C10PropBorrowAA11LargeNCPropVvb : $@convention(method) (@in_guaranteed NCLargeStruct) -> @out LargeNCProp {
// CHECK: bb0([[REG0:%.*]] : $*LargeNCProp, [[REG1:%.*]] : $*NCLargeStruct):
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG1]], #NCLargeStruct._largeProp
// CHECK:   copy_addr [take] [[REG3]] to [init] [[REG0]]
// CHECK:   [[REG5:%.*]] = tuple ()
// CHECK:   return [[REG5]]
// CHECK: }

// CHECK-IRGEN: define hidden swiftcc void @"$s21borrow_accessor_large13NCLargeStructV0C10PropBorrowAA11LargeNCPropVvb"(ptr noalias nocapture sret(%T21borrow_accessor_large11LargeNCPropV) [[REG0]], ptr noalias nocapture swiftself dereferenceable(72) [[REG1]]) {{.*}} {
// CHECK-IRGEN: entry:
// CHECK-IRGEN:   %._largeProp = getelementptr inbounds %T21borrow_accessor_large13NCLargeStructV, ptr [[REG1]], i32 0, i32 1
// CHECK-IRGEN:   call void @llvm.memcpy.p0.p0.i64(ptr align 8 [[REG0]], ptr align 8 %._largeProp, i64 64, i1 false)
// CHECK-IRGEN:   ret void
// CHECK-IRGEN: }

