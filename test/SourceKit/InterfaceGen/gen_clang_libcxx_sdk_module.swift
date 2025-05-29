// RUN: %sourcekitd-test -req=interface-gen -module CxxStdlib -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -cxx-interoperability-mode=default -target %target-triple -sdk %sdk | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: import {{CxxStdlib.vector|std_vector}}
// CHECK: extension std.basic_string<CChar, std.__1.char_traits<CChar>, std.__1.allocator<CChar>> {
