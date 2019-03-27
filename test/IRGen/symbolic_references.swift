// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -enable-private-imports %s | %FileCheck %s --check-prefix=PRIVATE

protocol P {
  associatedtype A
}
// CHECK: @"symbolic _____ 19symbolic_references3Foo016_{{.*}}V5InnerV9InnermostV" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1,
// CHECK: @"symbolic _____ 19symbolic_references3Foo016_{{.*}}V5InnerV" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1
// CHECK: @"symbolic _____ 19symbolic_references3Foo016_{{.*}}V" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1

// PRIVATE: @"symbolic _____ 19symbolic_references3Foo016_{{.*}}V5InnerV9InnermostV" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 2
// PRIVATE: @"symbolic _____ 19symbolic_references3Foo016_{{.*}}V5InnerV" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1
// PRIVATE: @"symbolic _____ 19symbolic_references3Foo016_{{.*}}V" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1
fileprivate struct Foo {
  fileprivate struct Inner: P {
    fileprivate struct Innermost { }
    typealias A = Innermost
  }
}
