// RUN: %target-swift-frontend -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -emit-sil %s | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials

class C {}

// CHECK: sil @$e11existential4testyyF
// CHECK: init_existential_addr
// CHECK: } // end sil function '$e11existential4testyyF'

func test() {
  let any: any Any = C()
}

@main
struct Main {
  static func main() {
    test()
  }
}
