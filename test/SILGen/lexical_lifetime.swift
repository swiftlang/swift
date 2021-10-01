// RUN: %target-swift-emit-silgen -enable-experimental-lexical-lifetimes -module-name borrow -parse-stdlib %s | %FileCheck %s

import Swift

////////////////////////////////////////////////////////////////////////////////
// Declarations                                                               {{
////////////////////////////////////////////////////////////////////////////////

final class C {
  init() {}
  init?(failably: ()) {}
}

struct S {
  let c: C
}

struct Trivial {
  let i: Int
}

enum E {
  case e(C)
}

@_silgen_name("use_generic")
func use_generic<T>(_ t: T) {}

////////////////////////////////////////////////////////////////////////////////
// Declarations                                                               }}
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Tests                                                                      {{
////////////////////////////////////////////////////////////////////////////////

// let bindings:

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_let_class
// CHECK:   [[INIT_C:%[^,]+]] = function_ref @$s6borrow1CCACycfC
// CHECK:   [[INSTANCE:%[^,]+]] = apply [[INIT_C]]({{%[0-9]+}})
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $C
// CHECK:   end_borrow [[BORROW:%[^,]+]]
// CHECK-LABEL: } // end sil function 'lexical_borrow_let_class'
@_silgen_name("lexical_borrow_let_class")
func lexical_borrow_let_class() {
  let c = C()
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_if_let_class
// CHECK:   [[INIT_C:%[^,]+]] = function_ref @$s6borrow1CC8failablyACSgyt_tcfC
// CHECK:   [[INSTANCE:%[^,]+]] = apply [[INIT_C]]({{%[^,]+}})
// CHECK:   switch_enum [[INSTANCE]] : $Optional<C>, case #Optional.some!enumelt: [[BASIC_BLOCK2:bb[^,]+]], case #Optional.none!enumelt: {{bb[^,]+}}
// CHECK: [[BASIC_BLOCK2]]([[INSTANCE:%[^,]+]] : @owned $C):
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $C
// CHECK:   end_borrow [[BORROW]] : $C
// CHECK-LABEL: // end sil function 'lexical_borrow_if_let_class'
@_silgen_name("lexical_borrow_if_let_class")
func lexical_borrow_if_let_class() {
  if let c = C(failably: ()) {
    use_generic(())
  }
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_let_class_in_struct
// CHECK:   [[INIT_S:%[^,]+]] = function_ref @$s6borrow1SV1cAcA1CC_tcfC
// CHECK:   [[INSTANCE:%[^,]+]] = apply [[INIT_S]]({{%[0-9]+}}, {{%[0-9]+}})
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $S
// CHECK:   end_borrow [[BORROW:%[^,]+]]
// CHECK-LABEL: } // end sil function 'lexical_borrow_let_class_in_struct'
@_silgen_name("lexical_borrow_let_class_in_struct")
func lexical_borrow_let_class_in_struct() {
  let s = S(c: C())
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_let_class_in_enum
// CHECK:   [[INSTANCE:%[^,]+]] = enum $E, #E.e!enumelt, {{%[0-9]+}} : $C
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $E
// CHECK:   end_borrow [[BORROW:%[^,]+]]
// CHECK-LABEL: } // end sil function 'lexical_borrow_let_class_in_enum'
@_silgen_name("lexical_borrow_let_class_in_enum")
func lexical_borrow_let_class_in_enum() {
  let s = E.e(C())
}

// arguments:

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_arg_guaranteed_class : $@convention(thin) (@guaranteed C) -> () {
// CHECK:       {{bb[^,]+}}([[INSTANCE:%[^,]+]] : @guaranteed $C):
// CHECK:         [[LIFETIME:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] 
// CHECK:         debug_value [[LIFETIME]] 
// CHECK:         [[ADDR:%[^,]+]] = alloc_stack $C
// CHECK:         store_borrow [[LIFETIME]] to [[ADDR]] 
// CHECK:         [[USE_GENERIC:%[^,]+]] = function_ref @use_generic
// CHECK:         [[REGISTER_6:%[^,]+]] = apply [[USE_GENERIC]]<C>([[ADDR]]) 
// CHECK:         dealloc_stack [[ADDR]] 
// CHECK:         end_borrow [[LIFETIME]] 
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]] 
// CHECK-LABEL: } // end sil function 'lexical_borrow_arg_guaranteed_class'
@_silgen_name("lexical_borrow_arg_guaranteed_class")
func lexical_borrow_arg_guaranteed_class(_ c: C) {
  use_generic(c)
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_arg_class_addr : $@convention(thin) (@inout C) -> () {
// CHECK-NOT:     begin_borrow [lexical]
// CHECK-LABEL: } // end sil function 'lexical_borrow_arg_class_addr'
@_silgen_name("lexical_borrow_arg_class_addr")
func lexical_borrow_arg_class_addr(_ c: inout C) {
  use_generic(c)
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_arg_trivial : $@convention(thin) (Trivial) -> () {
// CHECK-NOT:     begin_borrow [lexical]
// CHECK-LABEL: } // end sil function 'lexical_borrow_arg_trivial'
@_silgen_name("lexical_borrow_arg_trivial")
func lexical_borrow_arg_trivial(_ trivial: Trivial) {
  use_generic(trivial)
}

////////////////////////////////////////////////////////////////////////////////
// Test                                                                       }}
////////////////////////////////////////////////////////////////////////////////

