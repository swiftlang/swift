// RUN: %empty-directory(%t)
// RUN: %target-clangxx %S/../../Interop/Cxx/stdlib/Inputs/check-libcxx-version.cpp -o %t/check-libcxx-version
// RUN: %target-codesign %t/check-libcxx-version

// Since this test runs check-libcxx-version, it requires execution.
// REQUIRES: executable_test

// RUN: %target-run %t/check-libcxx-version || %sourcekitd-test -req=interface-gen -module CxxStdlib -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=default -target %target-triple -sdk %sdk | %FileCheck %s --check-prefix CHECK-MONO
// RUN: not %target-run %t/check-libcxx-version || %sourcekitd-test -req=interface-gen -module CxxStdlib -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=default -target %target-triple -sdk %sdk | %FileCheck %s --check-prefix CHECK-SPLIT

// REQUIRES: OS=macosx

// CHECK-MONO-NOT: import CxxStdlib{{$}}
// CHECK-MONO: import CxxStdlib.vector
// CHECK-MONO: extension std.basic_string<CChar, std.__1.char_traits<CChar>, std.__1.allocator<CChar>> {

// CHECK-SPLIT-NOT: import CxxStdlib{{$}}
// CHECK-SPLIT: import std_vector
// CHECK-SPLIT: extension std.basic_string<CChar, std.__1.char_traits<CChar>, std.__1.allocator<CChar>> {
