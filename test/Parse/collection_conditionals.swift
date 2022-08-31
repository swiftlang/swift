// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck --strict-whitespace %s

let a4: [Any] = [
// CHECK:  (array_expr
  #if true
    2.0,
// CHECK:     (float_literal_expr {{.*}} value=2.0
    #if false
      3.0,
// CHECK-NOT: (float_literal_expr {{.*}} value=3.0
    #elseif true
      [55, 44],
// CHECK:     (array_expr type=
// CHECK:       (integer_literal_expr {{.*}} value=55
// CHECK:       (integer_literal_expr {{.*}} value=44
    #else
      6, 7,
// CHECK-NOT: (integer_literal_expr {{.*}} value=6
// CHECK-NOT: (integer_literal_expr {{.*}} value=7
    #endif
  #endif
  32,
// CHECK:     (integer_literal_expr {{.*}} value=32
]

let d2: [String: Int] = [
// CHECK:  (dictionary_expr
  #if false
    "b22": 22,
// CHECK-NOT: (tuple_expr implicit
  #endif
]
