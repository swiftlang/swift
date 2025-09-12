// RUN: %sourcekitd-test -req=interface-gen -module CxxStdlib -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=default -target %target-triple -sdk %sdk | %FileCheck %s
// RUN: %sourcekitd-test -req=interface-gen -module CxxStdlib.vector -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=default -target %target-triple -sdk %sdk | %FileCheck %s --check-prefix CHECK-VECTOR --implicit-check-not import

// REQUIRES: OS=macosx

// CHECK: import {{CxxStdlib.vector|std_vector}}
// CHECK: extension std.basic_string<CChar, std.__1.char_traits<CChar>, std.__1.allocator<CChar>> {

// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.comparison
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.container_traits
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.erase
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.fwd
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.pmr
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.swap
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.vector
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.vector_bool
// CHECK-VECTOR: import {{CxxStdlib.vector|std_vector}}.vector_bool_formatter
