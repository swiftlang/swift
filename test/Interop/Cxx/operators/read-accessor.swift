// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MemberInline

var arr = NonTrivialArrayByRef()
let _ = arr[0].f + 1;

// Verify that a read accessor is generated, and that the yielded non trivial type is not copied.

// CHECK:     // main
// CHECK:     sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK-NOT: alloc_stack $NonTrivial
// CHECK:     [[OP_REF:%[0-9]+]] = function_ref {{@.*NonTrivialArrayByRef.*}} : $@convention(cxx_method) (Int32, @inout NonTrivialArrayByRef) -> UnsafeMutablePointer<NonTrivial>
// CHECK:     [[RESULT_POINTER:%[0-9]+]] = apply [[OP_REF]]({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(cxx_method) (Int32, @inout NonTrivialArrayByRef) -> UnsafeMutablePointer<NonTrivial>
// CHECK:     [[RESULT_ADDRESS:%[0-9]+]] = struct_extract [[RESULT_POINTER]] : $UnsafeMutablePointer<NonTrivial>, #UnsafeMutablePointer._rawValue
// CHECK:     [[PTR_TO_ADDR:%[0-9]+]] = pointer_to_address [[RESULT_ADDRESS]] : $Builtin.RawPointer to [strict] $*NonTrivial
// CHECK:     [[ACCESS:%[0-9]+]] = begin_access [read] [unsafe] [[PTR_TO_ADDR]] : $*NonTrivial
// CHECK:     [[F_ADDR:%[0-9]+]] = struct_element_addr [[ACCESS]] : $*NonTrivial, #NonTrivial.f
// CHECK:     {{%[0-9]+}} = load [[F_ADDR]] : $*Int32
// CHECK:     // end sil function 'main'

// CHECK: // NonTrivialArrayByRef.subscript.read
// CHECK: sil shared [transparent] @${{.*NonTrivialArrayByRef.*}} : $@yield_once @convention(method) (Int32, @inout NonTrivialArrayByRef) -> @yields @in_guaranteed NonTrivial
// CHECK-NOT: copy_addr {{%[0-9]+}} to [initialization] {{%[0-9]+}} : $*NonTrivial
