// RUN: %target-swift-emit-silgen %s -verify | %FileCheck %s

// Tests for a crash that occurred when the result builder transform encountered
// an empty case statement.
protocol V { }

struct EV: V { }

@resultBuilder
struct VB {
  static func buildBlock(_ components: any V...) -> any V { EV() }
  static func buildEither(first: any V) -> any V { first }
  static func buildEither(second: any V) -> any V { second }
}

extension String: V { }

enum E {
    case a(Int)
    case b(String)
}

struct S {
  var flag: E

  // CHECK-LABEL: sil hidden [ossa] @$s25result_builder_empty_case1SV4testAA1V_pyF
  // CHECK: switch_enum
  @VB
  func test() -> any V {
    switch flag {
    case .a:
      // When NOT_DEFINED is... not defined... this ends up being an empty case.
      // We permit this
#if NOT_DEFINED
      EV()
#endif
    case .b:
      EV()
    }
  }
}
