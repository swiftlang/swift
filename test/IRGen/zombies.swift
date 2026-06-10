// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s

// Note: Windows uses internal linkage, which puts an extra step symbol before
// _swift_dead_method_stub.
// UNSUPPORTED: OS=windows-msvc

// rdar://24121475
//   Ideally, these wouldn't be in the v-table at all; but as long as they
//   are, we need to emit symbols for them.
class C {
  private var i: Int
  init(i: Int) { self.i = i }
}

// CHECK: @"$s7zombies1CC1i33_{{.*}}vs" = hidden {{(dllexport )?}}alias void (), ptr @_swift_dead_method_stub

// CHECK: define {{(linkonce_odr )?}}hidden void @_swift_dead_method_stub
// CHECK: entry:
// CHECK:  tail call void @swift_deletedMethodError()
