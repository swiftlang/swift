// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-sil %s -verify

protocol Summable_2: ~Copyable {
  static var zero: Self { get }
}

struct NCInt_2: ~Copyable, Summable_2 {
  var rawValue: Int

  static var zero: Self {
    .init(rawValue: 0)
  }
}

// CHECK-LABEL: sil {{.*}} [ossa] @$s13rdar1290102657NCInt_2VAA10Summable_2A2aDP4zeroxvyZTW : {{.*}} {
// CHECK:         [[METATYPE:%[^,]+]] = metatype $@thin NCInt_2.Type
// CHECK:         [[IMPL:%[^,]+]] = function_ref @$s13rdar1290102657NCInt_2V4zeroACvyZ
// CHECK:         ([[ZERO:%[^,]+]], [[TOKEN:%[^,]+]], [[STCK:%[^,]+]]) = begin_apply [[IMPL]]([[METATYPE]])
// CHECK:         [[ALLOC:%[^,]+]] = alloc_stack $NCInt_2
// CHECK:         [[MARK:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[ALLOC]]
// CHECK:         [[ADDR:%[^,]+]] = store_borrow [[ZERO]] to [[MARK]]
// CHECK:         yield [[ADDR:%[^,]+]]
// CHECK:             resume [[RESUME:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[RESUME]]:
// CHECK:         end_borrow [[ADDR]]
// CHECK:         dealloc_stack [[ALLOC]]
// CHECK:         end_apply [[TOKEN]] as $()
// CHECK:         dealloc_stack [[STCK]]

// CHECK:       [[UNWIND]]:
// CHECK:         end_borrow [[ADDR]]
// CHECK:         dealloc_stack [[ALLOC]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[STCK]]
// CHECK-LABEL: } // end sil function '$s13rdar1290102657NCInt_2VAA10Summable_2A2aDP4zeroxvyZTW'
