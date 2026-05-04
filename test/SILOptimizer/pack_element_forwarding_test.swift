// RUN: %target-swift-frontend -parse-as-library -O -target %module-target-future -emit-sil %s | %FileCheck %s

@resultBuilder
struct ViewBuilderParameterPack {
    @inline(always)
    static func buildExpression<Content: View>(_ content: Content) -> Content {
       content
    }

    @inline(always)
    static func buildBlock<Content: View>(_ content: Content) -> Content {
        content
    }

    @_disfavoredOverload
    @inline(always)
    static func buildBlock<each Content: View>(_ content: repeat each Content) -> TupleView<(repeat each Content)> {
        TupleView((repeat each content))
    }
}

public struct TupleView<each Content>: View {
    var content: (repeat each Content)

    @inline(always)
    init(_ content: (repeat each Content)) {
        self.content = content
    }

    @_disfavoredOverload
    @inline(always)
    init(_ content: repeat each Content) {
        self.init((repeat each content))
    }
}

@ViewBuilderParameterPack
public func bodyParameterPacks2() -> some View {
    MyView()
    MyView()
}

final class MyClass {}

public protocol View {}
public struct MyView: View {
    var storage: InlineArray<10, MyClass> = .init(repeating: MyClass())
    @inline(never)
    init() {}
}

// CHECK-LABEL: sil @$s28pack_element_forwarding_test19bodyParameterPacks2QryF
// CHECK-NOT: alloc_pack
// CHECK-NOT: pack_element_set
// CHECK-NOT: pack_element_get
// CHECK-NOT: dealloc_pack
// CHECK: } // end sil function '$s28pack_element_forwarding_test19bodyParameterPacks2QryF'
