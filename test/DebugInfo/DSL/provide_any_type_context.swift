// RUN: %target-swift-frontend %s -Xllvm -sil-print-debuginfo -emit-sil -g -Onone -disable-availability-checking | %FileCheck %s

struct B {
  var value: Int
}

struct A {
  var value: B
}

@resultBuilder
enum SimpleBuilder {
  struct Component {
    let value: Int
    let debugInfoProvider: DSLDebugInfoProvider?
  }

  static func buildExpression(
    _ expression: Int
  ) -> Component {
    .init(value: expression, debugInfoProvider: nil)
  }

  static func buildDebuggable(
    component: Component,
    debugInfoProvider: DSLDebugInfoProvider
  ) -> Component {
    .init(value: component.value, debugInfoProvider: debugInfoProvider)
  }

  static func buildBlock(_ components: Component...) -> [Component] {
    return components
  }
}

func build(
  @SimpleBuilder make: () -> [SimpleBuilder.Component]
) -> [SimpleBuilder.Component] {
  make()
}

var thing = A(value: B(value: 1))

// TEST: ALSO FIELD ACCESS
let useGlobalVar = build {
  thing.value.value
  A(value: B(value: 2)).value.value
  A(value: B(value: 3)).value.value
}

useGlobalVar[2].debugInfoProvider?(
  context: [useGlobalVar[2].value * 2, 1441],
  stackTrace: [
    .init(provider: useGlobalVar[0].debugInfoProvider!,
          context: Double(useGlobalVar[0].value * 2)),
    .init(provider: useGlobalVar[1].debugInfoProvider!,
          context: useGlobalVar[1].value * 2),
  ]
)



/*
 
 can check sil at same time,
 ~/swift-project/swift/test/AutoDiff/IRGen/loadable_by_address.swift
 
 Also import StdlibUnittest
 
 // RUN: %target-run-simple-swift %s
 // REQUIRES: executable_test
 // RUN: %target-run-simple-swift(-parse-as-library) | %FileCheck %s
 // REQUIRES: executable_test

 // CHECK: message printed
 */

// CHECK-LABEL: @$s24provide_any_type_contextSayAA13SimpleBuilderO9ComponentVGyXEfU_AEff_
// CHECK: debug_value %0 : $*Any, let, name "dslDebugInfo", argno 1, implicit, expr op_deref, loc
