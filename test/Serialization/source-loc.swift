// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_source_loc.swift -experimental-serialize-debug-info -O -g
// RUN: llvm-bcanalyzer %t/def_source_loc.swiftmodule | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -module-name source_loc -g -emit-sil -I %t %s | %FileCheck %s

// SIL-NOT: UnknownCode

import def_source_loc

let _ = foo(x: 100)

//CHECK: {{.*integer_literal.*}} loc "{{.*}}def_source_loc.swift":3:13
//CHECK: {{.*struct_extract.*}} loc "{{.*}}def_source_loc.swift":3:11
//CHECK: {{.*builtin "cmp_ult_Int64".*}} loc "{{.*}}def_source_loc.swift":3:11
//CHECK: {{.*cond_br.*}} loc "{{.*}}def_source_loc.swift":3:11
//CHECK: {{.*integer_literal.*}} 1, loc "{{.*}}def_source_loc.swift":7:12
//CHECK: {{.*br.*}} loc "{{.*}}def_source_loc.swift":7:5
//CHECK: {{.*struct \$UInt64.*}} loc "{{.*}}def_source_loc.swift":7:12
//CHECK: {{.*return.*}} loc "{{.*}}def_source_loc.swift":8:2
