// RUN: %target-swift-frontend -module-name=Hello -emit-ir %s | %FileCheck %s
// REQUIRES: OS=macosx

// Check if we mangle the objc runtime name correctly if a class has the same name as the module.

class Hello {}

// CHECK-DAG: _METACLASS_DATA__TtC5Hello5Hello
// CHECK-DAG: c"_TtC5Hello5Hello\00"
// CHECK-DAG: _DATA__TtC5Hello5Hello

