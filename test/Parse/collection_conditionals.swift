// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck --strict-whitespace %s

let a4: [Any] = [
  #if true
// CHECK: #if: active
// CHECK:  (boolean_literal_expr type='<null>' value=true)
// CHECK:  (elements
    2.0,
// CHECK:    (float_literal_expr type='<null>' value=2.0)
    #if false
// CHECK: #if:
// CHECK:  (boolean_literal_expr type='<null>' value=false)
// CHECK:  (elements
      3.0,
// CHECK:    (float_literal_expr type='<null>' value=3.0))
    #elseif true
// CHECK:  #if: active
// CHECK:    (boolean_literal_expr type='<null>' value=true)
// CHECK:    (elements
      [55, 44],
    #else
// CHECK:  #else:
// CHECK:    (elements
      6, 7,
// CHECK:      (integer_literal_expr type='<null>' value=6)
// CHECK:      (integer_literal_expr type='<null>' value=7)))))
    #endif
  #endif
  32,
]

let d2 = [
  #if false
// CHECK:  #if:
// CHECK:    (boolean_literal_expr type='<null>' value=false)
// CHECK:    (elements
    "b22": 22,
// CHECK:      (tuple_expr implicit type='<null>'
// CHECK:        (string_literal_expr type='<null>' encoding=utf8 value="b22" builtin_initializer=**NULL** initializer=**NULL**)
// CHECK:        (integer_literal_expr type='<null>' value=22))))
  #endif
  "c": 3,
]
