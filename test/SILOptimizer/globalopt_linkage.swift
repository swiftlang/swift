// RUN: %target-swift-frontend  -O -Xllvm -sil-disable-pass=inline -emit-sil -primary-file %s | %FileCheck %s

// Check if GlobalOpt generates the getters with the right linkage and the right mangling

struct MyStruct {
  static let StaticVar = 10
}

let Global = 27

func testit() -> Int {
  return MyStruct.StaticVar + Global + PublicGlobal
}

public let PublicGlobal = 27

_ = testit()

// CHECK: sil hidden @{{.*}}testit

// CHECK:      // MyStruct.StaticVar.getter
// CHECK-NEXT: sil private @$s{{.*}}StaticVar

// CHECK:      // Global.getter
// CHECK-NEXT: sil private @$s{{.*}}Global

// CHECK:      // PublicGlobal.getter
// CHECK-NEXT: sil non_abi @$s{{.*}}PublicGlobal
