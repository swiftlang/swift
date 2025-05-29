// REQUIRES: VENDOR=apple 
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -enable-testing -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd -tbd-install_name test
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd -tbd-install_name  test
// RUN: %llvm-readtapi --compare  %t/typecheck.tbd %t/emit-ir.tbd

public protocol DefaultInit {
  init()
}

@available(macOS 10.15, *)
struct X {
  subscript<T: DefaultInit>(_ value: Int = 0, other: T = T()) -> Int { value + 1 }

  static subscript<T>(_ value: String = "hello", other: T) -> String {
    get { value }
    set { }
  }
}

