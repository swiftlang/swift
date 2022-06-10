// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -debug-constraints > %t.log 2>&1
// RUN: %FileCheck %s < %t.log

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

protocol View {}
protocol Callable {}

struct EmptyView : View {}

@resultBuilder struct ViewBuilder {
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View { fatalError() }
}

extension Callable {
  func callAsFunction<T: View>(@ViewBuilder _: () -> T) -> some View { EmptyView() }
}

struct MyView<Content> : View {
  init(v: Int, @ViewBuilder _: () -> Content) {}
}

extension MyView : Callable where Content == EmptyView {
  init(v: Int) {}
}

// CHECK: (overload set choice binding $T6 := (Int) -> MyView<{{.*}}>)
// CHECK-NEXT: (increasing score due to disfavored overload)
// CHECK-NEXT: (solution is worse than the best solution)

func test() -> some View {
  return MyView(v: 42) {
    return EmptyView()
  }
}
