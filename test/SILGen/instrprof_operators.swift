// RUN: %target-swift-frontend -parse-as-library -emit-silgen -profile-generate %s | FileCheck %s

// CHECK: sil hidden @[[F_OPERATORS:.*operators.*]] :
// CHECK: %[[NAME:.*]] = string_literal utf8 "[[F_OPERATORS]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 4
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 0
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
func operators(a : Bool, b : Bool) {
  let c = a && b
  let d = a || b

// CHECK: %[[NAME:.*]] = string_literal utf8 "[[F_OPERATORS]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 4
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 3
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
  let e = c ? a : b

  // CHECK-NOT: builtin "int_instrprof_increment"
}

// CHECK: implicit closure
// CHECK: %[[NAME:.*]] = string_literal utf8 "[[F_OPERATORS]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 4
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 1
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
// CHECK-NOT: builtin "int_instrprof_increment"

// CHECK: implicit closure
// CHECK: %[[NAME:.*]] = string_literal utf8 "[[F_OPERATORS]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 4
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 2
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
// CHECK-NOT: builtin "int_instrprof_increment"
