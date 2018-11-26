// RUN: %target-swift-frontend -parse-as-library -enable-sil-ownership -emit-silgen -profile-generate %s | %FileCheck %s

// CHECK: sil hidden @[[F_EMPTY:.*empty.*]] :
// CHECK: %[[NAME:.*]] = string_literal utf8 "{{.*}}instrprof_basic.swift:[[F_EMPTY]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 1
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 0
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
func empty() {
  // CHECK-NOT: builtin "int_instrprof_increment"
}

// CHECK: sil hidden @[[F_BASIC:.*basic.*]] :
// CHECK: %[[NAME:.*]] = string_literal utf8 "{{.*}}instrprof_basic.swift:[[F_BASIC]]"
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
  for i in 0 ..< a {
  }

  // CHECK: builtin "int_instrprof_increment"
  for i in 1...a {
  }

  // CHECK-NOT: builtin "int_instrprof_increment"
}

// CHECK: sil hidden @[[F_THROWING_NOP:.*throwing_nop.*]] :
func throwing_nop() throws {}
// CHECK: sil hidden @[[F_EXCEPTIONS:.*exceptions.*]] :
// CHECK: %[[NAME:.*]] = string_literal utf8 "{{.*}}instrprof_basic.swift:[[F_EXCEPTIONS]]"
// CHECK: %[[HASH:.*]] = integer_literal $Builtin.Int64,
// CHECK: %[[NCOUNTS:.*]] = integer_literal $Builtin.Int32, 2
// CHECK: %[[INDEX:.*]] = integer_literal $Builtin.Int32, 0
// CHECK: builtin "int_instrprof_increment"(%[[NAME]] : {{.*}}, %[[HASH]] : {{.*}}, %[[NCOUNTS]] : {{.*}}, %[[INDEX]] : {{.*}})
func exceptions() {
  do {
    try throwing_nop()
  } catch {
    // CHECK: builtin "int_instrprof_increment"
    return
  }

  // CHECK-NOT: builtin "int_instrprof_increment"
}
