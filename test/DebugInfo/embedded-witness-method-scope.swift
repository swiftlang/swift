// Specializing a witness_method for embedded Swift must preserve the
// instruction's lexical debug scope. 
//
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil \
// RUN:   -Xllvm -sil-print-debuginfo -target %target-cpu-apple-macos14 \
// RUN:   -enable-experimental-feature Embedded -parse-as-library -wmo -Onone -g \
// RUN:   -module-name main %s -o - | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

protocol P {
  mutating func f() -> Int
}

final class C: P {
  func f() -> Int { return 42 }
}

public func test() {
  var p: P = C()
  _ = p.f()
}

// The body of test() opens a lexical block for `p`, whose scope has the
// function's scope as its parent.
// CHECK: sil_scope [[FUNC:[0-9]+]] { {{.*}} parent @$e4main4testyyF
// CHECK: sil_scope [[BLOCK:[0-9]+]] { {{.*}} parent [[FUNC]] }

// The specialized witness_method and the apply that consumes it must both sit
// in that lexical block, not in the function scope.
// CHECK: witness_method {{.*}}#P.f{{.*}}, scope [[BLOCK]]
// CHECK: apply {{.*}}$@convention(witness_method: P){{.*}}, scope [[BLOCK]]
