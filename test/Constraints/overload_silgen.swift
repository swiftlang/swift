// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// CHECK: sil hidden @$S15overload_silgen8anyToIntySiypF
// CHECK: end sil function '$S15overload_silgen8anyToIntySiypF'
func anyToInt(_: Any) -> Int { fatalError() }

// CHECK: sil hidden @$S15overload_silgen8anyToIntySiSgypSgF
// CHECK: function_ref @$S15overload_silgen8anyToIntySiypF
// CHECK: end sil function '$S15overload_silgen8anyToIntySiSgypSgF'
func anyToInt(_ value: Any?) -> Int? { return anyToInt(value!) }
