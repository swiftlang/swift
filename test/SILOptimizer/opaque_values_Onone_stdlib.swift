// RUN: %target-swift-frontend -parse-stdlib -enable-experimental-move-only -module-name main -enable-sil-opaque-values -parse-as-library -emit-sil -Onone %s | %FileCheck %s

// Like opaque_values_Onone.swift but for code that needs to be compiled with 
// -parse-stdlib.

class X {}

func consume(_ x : __owned X) {}

func foo(@_noImplicitCopy _ x: __owned X) {
  consume(_copy(x))
  consume(x)
}

// CHECK-LABEL: sil [transparent] [_semantics "lifetimemanagement.copy"] @_copy : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[OUT_ADDR:%[^,]+]] : $*T, [[IN_ADDR:%[^,]+]] : $*T):
// CHECK:         [[TMP_ADDR:%[^,]+]] = alloc_stack $T
// CHECK:         copy_addr [[IN_ADDR]] to [init] [[TMP_ADDR]] : $*T
// CHECK:         [[REGISTER_5:%[^,]+]] = builtin "copy"<T>([[OUT_ADDR]] : $*T, [[TMP_ADDR]] : $*T) : $()
// CHECK:         dealloc_stack [[TMP_ADDR]] : $*T
// CHECK:         return {{%[^,]+}} : $()
// CHECK-LABEL: } // end sil function '_copy'
@_transparent
@_semantics("lifetimemanagement.copy")
@_silgen_name("_copy")
public func _copy<T>(_ value: T) -> T {
  #if $BuiltinCopy
    Builtin.copy(value)
  #else
    value
  #endif
}


