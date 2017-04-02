// RUN: %target-swift-frontend  -O -Xllvm -sil-disable-pass=Inliner -emit-sil -primary-file %s | %FileCheck %s

// Check if GlobalOpt generates the getters with the right linkage and the right mangling

struct MyStruct {
  static let StaticVar = 10
}

let Global = 27

func testit() -> Int {
  return MyStruct.StaticVar + Global
}

_ = testit()

// CHECK: sil hidden @{{.*}}testit

// CHECK:      // MyStruct.StaticVar.getter
// CHECK-NEXT: sil private [serialized] @_{{.*}}StaticVar

// CHECK:      // Global.getter
// CHECK-NEXT: sil private [serialized] @_{{.*}}Global
