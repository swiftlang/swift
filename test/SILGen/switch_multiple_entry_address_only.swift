
// RUN: %target-swift-emit-silgen -module-name switch_multiple_entry_address_only %s | %FileCheck %s

enum E {
case a(Any)
case b(Any)
case c(Any)
}

// CHECK-LABEL: sil hidden @$s34switch_multiple_entry_address_only8takesAnyyyypF : $@convention(thin) (@in_guaranteed Any) -> ()
func takesAny(_ x: Any) {}

// CHECK-LABEL: sil hidden @$s34switch_multiple_entry_address_only0B9LabelsLet1eyAA1EO_tF : $@convention(thin) (@in_guaranteed E) -> ()
func multipleLabelsLet(e: E) {
  // CHECK:      bb0
  // CHECK:      [[X_PHI:%.*]] = alloc_stack $Any
  // CHECK-NEXT: [[E_COPY:%.*]] = alloc_stack $E
  // CHECK-NEXT: copy_addr %0 to [initialization] [[E_COPY]]
  // CHECK-NEXT: switch_enum_addr [[E_COPY]] : $*E, case #E.a!enumelt.1: bb1, case #E.b!enumelt.1: bb2, default bb4

  // CHECK:      bb1:
  // CHECK-NEXT: [[E_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[E_COPY]] : $*E, #E.a!enumelt.1
  // CHECK-NEXT: [[ANY_BOX:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [take] [[E_PAYLOAD]] to [initialization] [[ANY_BOX]]
  // CHECK-NEXT: copy_addr [[ANY_BOX]] to [initialization] [[X_PHI]]
  // CHECK-NEXT: destroy_addr [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb3

  // CHECK:      bb2:
  // CHECK-NEXT: [[E_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[E_COPY]] : $*E, #E.b!enumelt.1
  // CHECK-NEXT: [[ANY_BOX:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [take] [[E_PAYLOAD]] to [initialization] [[ANY_BOX]]
  // CHECK-NEXT: copy_addr [[ANY_BOX]] to [initialization] [[X_PHI]]
  // CHECK-NEXT: destroy_addr [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb3

  // CHECK:      bb3:
  // CHECK:      [[FN:%.*]] = function_ref @$s34switch_multiple_entry_address_only8takesAnyyyypF
  // CHECK-NEXT: apply [[FN]]([[X_PHI]]
  // CHECK-NEXT: destroy_addr [[X_PHI]]
  // CHECK-NEXT: br bb6

  // CHECK:      bb4:
  // CHECK-NEXT: br bb5

  // CHECK:      bb5:
  // CHECK-NEXT: destroy_addr [[E_COPY]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb6

  // CHECK:      bb6:
  // CHECK-NEXT: dealloc_stack [[X_PHI]]
  // CHECK-NEXT: tuple ()
  // CHECK-NEXT: return

  switch e {
  case .a(let x), .b(let x):
    takesAny(x)
  default:
    break
  }
}

// CHECK-LABEL: sil hidden @$s34switch_multiple_entry_address_only0B9LabelsVar1eyAA1EO_tF : $@convention(thin) (@in_guaranteed E) -> ()
func multipleLabelsVar(e: E) {
  // CHECK:      bb0
  // CHECK:      [[X_PHI:%.*]] = alloc_stack $Any
  // CHECK-NEXT: [[E_COPY:%.*]] = alloc_stack $E
  // CHECK-NEXT: copy_addr %0 to [initialization] [[E_COPY]]
  // CHECK-NEXT: switch_enum_addr [[E_COPY]] : $*E, case #E.a!enumelt.1: bb1, case #E.b!enumelt.1: bb2, default bb4

  // CHECK:      bb1:
  // CHECK-NEXT: [[E_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[E_COPY]] : $*E, #E.a!enumelt.1
  // CHECK-NEXT: [[ANY_BOX:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [take] [[E_PAYLOAD]] to [initialization] [[ANY_BOX]]
  // CHECK-NEXT: copy_addr [[ANY_BOX]] to [initialization] [[X_PHI]]
  // CHECK-NEXT: destroy_addr [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb3

  // CHECK:      bb2:
  // CHECK-NEXT: [[E_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[E_COPY]] : $*E, #E.b!enumelt.1
  // CHECK-NEXT: [[ANY_BOX:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [take] [[E_PAYLOAD]] to [initialization] [[ANY_BOX]]
  // CHECK-NEXT: copy_addr [[ANY_BOX]] to [initialization] [[X_PHI]]
  // CHECK-NEXT: destroy_addr [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb3

  // CHECK:      bb3:
  // CHECK-NEXT: [[ANY_BOX:%.*]] = alloc_box ${ var Any }, var, name "x"
  // CHECK-NEXT: [[BOX_PAYLOAD:%.*]] = project_box [[ANY_BOX]] : ${ var Any }, 0
  // CHECK-NEXT: copy_addr [take] [[X_PHI]] to [initialization] [[BOX_PAYLOAD]]
  // CHECK-NEXT: [[ACCESS:%.*]] = begin_access [read] [unknown] [[BOX_PAYLOAD]]
  // CHECK-NEXT: [[ANY_STACK:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [[ACCESS]] to [initialization] [[ANY_STACK]]
  // CHECK-NEXT: end_access [[ACCESS]]
  // CHECK:      [[FN:%.*]] = function_ref @$s34switch_multiple_entry_address_only8takesAnyyyypF
  // CHECK-NEXT: apply [[FN]]([[ANY_STACK]]
  // CHECK-NEXT: destroy_addr [[ANY_STACK]]
  // CHECK-NEXT: dealloc_stack [[ANY_STACK]]
  // CHECK-NEXT: destroy_value [[ANY_BOX]]
  // CHECK-NEXT: br bb6

  // CHECK:      bb4:
  // CHECK-NEXT: br bb5

  // CHECK:      bb5:
  // CHECK-NEXT: destroy_addr [[E_COPY]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb6

  // CHECK:      bb6:
  // CHECK-NEXT: dealloc_stack [[X_PHI]]
  // CHECK-NEXT: tuple ()
  // CHECK-NEXT: return

  switch e {
  case .a(var x), .b(var x):
    takesAny(x)
  default:
    break
  }
}

// CHECK-LABEL: sil hidden @$s34switch_multiple_entry_address_only20fallthroughWithValue1eyAA1EO_tF : $@convention(thin) (@in_guaranteed E) -> ()
func fallthroughWithValue(e: E) {
  // CHECK:      bb0
  // CHECK:      [[X_PHI:%.*]] = alloc_stack $Any
  // CHECK-NEXT: [[E_COPY:%.*]] = alloc_stack $E
  // CHECK-NEXT: copy_addr %0 to [initialization] [[E_COPY]]
  // CHECK-NEXT: switch_enum_addr [[E_COPY]] : $*E, case #E.a!enumelt.1: bb1, case #E.b!enumelt.1: bb2, default bb4
  
  // CHECK:      bb1:
  // CHECK-NEXT: [[E_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[E_COPY]] : $*E, #E.a!enumelt.1
  // CHECK-NEXT: [[ORIGINAL_ANY_BOX:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [take] [[E_PAYLOAD]] to [initialization] [[ORIGINAL_ANY_BOX]]
  // CHECK:      [[FN1:%.*]] = function_ref @$s34switch_multiple_entry_address_only8takesAnyyyypF
  // CHECK-NEXT: apply [[FN1]]([[ORIGINAL_ANY_BOX]]
  // CHECK-NEXT: copy_addr [[ORIGINAL_ANY_BOX]] to [initialization] [[X_PHI]]
  // CHECK-NEXT: destroy_addr [[ORIGINAL_ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[ORIGINAL_ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb3
  
  // CHECK:      bb2:
  // CHECK-NEXT: [[E_PAYLOAD:%.*]] = unchecked_take_enum_data_addr [[E_COPY]] : $*E, #E.b!enumelt.1
  // CHECK-NEXT: [[ANY_BOX:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [take] [[E_PAYLOAD]] to [initialization] [[ANY_BOX]]
  // CHECK-NEXT: copy_addr [[ANY_BOX]] to [initialization] [[X_PHI]]
  // CHECK-NEXT: destroy_addr [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[ANY_BOX]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb3
  
  // CHECK:      bb3:
  // CHECK:      [[FN2:%.*]] = function_ref @$s34switch_multiple_entry_address_only8takesAnyyyypF
  // CHECK-NEXT: apply [[FN2]]([[X_PHI]]
  // CHECK-NEXT: destroy_addr [[X_PHI]]
  // CHECK-NEXT: br bb6
  
  // CHECK:      bb4:
  // CHECK-NEXT: br bb5
  
  // CHECK:      bb5:
  // CHECK-NEXT: destroy_addr [[E_COPY]]
  // CHECK-NEXT: dealloc_stack [[E_COPY]]
  // CHECK-NEXT: br bb6
  
  // CHECK:      bb6:
  // CHECK-NEXT: dealloc_stack [[X_PHI]]
  // CHECK-NEXT: tuple ()
  // CHECK-NEXT: return
  
  switch e {
  case .a(let x):
    takesAny(x)
    fallthrough
  case .b(let x):
    takesAny(x)
  default:
    break
  }
}
