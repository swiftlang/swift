// RUN: %target-swift-ide-test -print-module -module-to-print=LocalClass -I %S/Inputs/ -source-filename=x -cxx-interoperability-mode=default 2>&1 | %FileCheck %s

// Classes declared inside a function body are not imported.

// CHECK-NOT: warning
// CHECK-NOT: struct Local
