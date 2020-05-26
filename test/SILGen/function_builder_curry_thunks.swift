// RUN: %target-swift-emit-silgen %s | %FileCheck %s

@_functionBuilder
struct Builder {
  static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
  }
}

struct Handler {
  var firstHandler: () -> ()
  var secondHandler: () -> ()
}

// We were neglecting to assign discriminators and re-parent
// autoclosures, which would manifest as curried method references
// producing a bogus diagnostic about captures from inside a
// nested type.
class Outer {
  struct Inner {
    @Builder
    var build: Handler {
      Handler(firstHandler: self.handler, secondHandler: self.handler)
    }

    private func handler() {}
  }
}

// CHECK-LABEL: sil private [ossa] @$s29function_builder_curry_thunks5OuterC5InnerV5buildAA7HandlerVvgyycAEcfu_ : $@convention(thin) (Outer.Inner) -> @owned @callee_guaranteed () -> ()
// CHECK-LABEL: sil private [ossa] @$s29function_builder_curry_thunks5OuterC5InnerV5buildAA7HandlerVvgyycAEcfu_yycfu0_ : $@convention(thin) (Outer.Inner) -> ()
// CHECK-LABEL: sil private [ossa] @$s29function_builder_curry_thunks5OuterC5InnerV7handler33_DC254A3F89F9C7E65D25434E199F17A4LLyyF : $@convention(method) (Outer.Inner) -> ()
// CHECK-LABEL: sil private [ossa] @$s29function_builder_curry_thunks5OuterC5InnerV5buildAA7HandlerVvgyycAEcfu1_ : $@convention(thin) (Outer.Inner) -> @owned @callee_guaranteed () -> ()