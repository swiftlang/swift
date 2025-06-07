// First, check that we error out for older C++ standards.
// RUN: not %sourcekitd-test -req=interface-gen -module UsesCXX20 -- -cxx-interoperability-mode=default -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -I %S/Inputs -target %target-triple -sdk %sdk

// Now make sure we emit the correct interface for std::span if C++20 is enabled.
// RUN: %sourcekitd-test -req=interface-gen -module UsesCXX20 -- -cxx-interoperability-mode=default -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -I %S/Inputs -target %target-triple -sdk %sdk -Xcc -std=c++20 | %FileCheck %s

// FIXME: older libstdc++ version on Swift CI does not support C++20
// UNSUPPORTED: OS=linux-gnu

// CHECK: public typealias MySpanOfInt = std.span<
