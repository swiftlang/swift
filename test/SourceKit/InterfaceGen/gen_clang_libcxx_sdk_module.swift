// RUN: %sourcekitd-test -req=interface-gen -module CxxStdlib -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=swift-5.9 -target %target-triple -sdk %sdk | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: import CxxStdlib.vector
// CHECK: extension std.basic_string<Int8, char_traits<Int8>, allocator<Int8>> {
