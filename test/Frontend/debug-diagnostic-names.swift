// RUN: not %target-swift-frontend -debug-diagnostic-names -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK_NAMES
// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK_NONAMES

let x =
// CHECK_NAMES: error: expected initial value after '=' [expected_init_value]{{$}}
// CHECK_NONAMES: error: expected initial value after '='{{$}}

// CHECK_NAMES: warning: expression following 'return' is treated as an argument of the 'return' [unindented_code_after_return]{{$}}
// CHECK_NAMES: note: indent the expression to silence this warning [indent_expression_to_silence]{{$}}
// CHECK_NONAMES: warning: expression following 'return' is treated as an argument of the 'return'{{$}}
// CHECK_NONAMES: note: indent the expression to silence this warning{{$}}
func foo() -> Int {
  return
  42
}

guard let y = 0 else {}
// CHECK_NAMES: error: initializer for conditional binding must have Optional type, not 'Int' [condition_optional_element_pattern_not_valid_type]{{$}}
// CHECK_NONAMES: error: initializer for conditional binding must have Optional type, not 'Int'{{$}}

let z: Double = ""
// CHECK_NAMES: error: cannot convert value of type 'String' to specified type 'Double' [cannot_convert_initializer_value]{{$}}
// CHECK_NONAMES: error: cannot convert value of type 'String' to specified type 'Double'{{$}}
