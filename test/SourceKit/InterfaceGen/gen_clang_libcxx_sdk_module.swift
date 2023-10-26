// RUN: %sourcekitd-test -req=interface-gen -module CxxStdlib -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=default -target %target-triple -sdk %sdk | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: import CxxStdlib.vector
// CHECK: extension std.basic_string<CChar, char_traits<CChar>, allocator<CChar>> {
