// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s

// rdar://24121475
//   Ideally, these wouldn't be in the v-table at all; but as long as they
//   are, we need to emit symbols for them.
class C {
  private var i: Int
  init(i: Int) { self.i = i }
}

// CHECK: @"$s7zombies1CC1i33_{{.*}}vs" = hidden {{(dllexport )?}}alias void (), ptr @"$s7zombies1CC1i33_45489CBEFBF369AB7AEE3A799A95D78DLLSivg"

// CHECK: define hidden void @"$s7zombies1CC1i33_45489CBEFBF369AB7AEE3A799A95D78DLLSivg"()
// CHECK: entry:
// CHECK:  tail call void @swift_deletedMethodError()
