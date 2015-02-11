// RUN: %target-swift-frontend -parse-as-library -emit-silgen -profile-generate %s | FileCheck %s

// CHECK: sil hidden @[[F_EMPTY:.*empty.*]] :
// CHECK: %[[NAME:.*]] = string_literal utf8 "[[F_EMPTY]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 1
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 0
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
func empty() {
  // CHECK-NOT: builtin "int_instrprof_increment"
}

// CHECK: sil hidden @[[F_BASIC:.*basic.*]] :
// CHECK: %[[NAME:.*]] = string_literal utf8 "[[F_BASIC]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 6
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 0
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
func basic(a : Int32) {

  // CHECK: builtin "int_instrprof_increment"
  if a == 0 {
  }

  // CHECK: builtin "int_instrprof_increment"
  if a != 0 {
  } else {
  }

  // CHECK: builtin "int_instrprof_increment"
  while a == 0 {
  }

  // CHECK: builtin "int_instrprof_increment"
  for var i: Int32 = 0; i < a; ++i {
  }

  // CHECK: builtin "int_instrprof_increment"
  for i in 1...a {
  }

  // CHECK-NOT: builtin "int_instrprof_increment"
}
