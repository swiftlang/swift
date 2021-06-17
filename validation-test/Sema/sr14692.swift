// RUN: %target-typecheck-verify-swift

enum Foo { case foo }
enum Bar { case bar }

@resultBuilder struct ViewBuilder2 {
  static func buildBlock(_ content: MyView) -> MyView { fatalError() }
  static func buildIf(_ content: MyView?) -> MyView { fatalError() }
}

func makeView(@ViewBuilder2 content: () -> MyView)  { fatalError() }

struct MyView {
  init() { fatalError() }
  
  func qadding(bar: Foo) -> MyView { fatalError() } // expected-note{{incorrect labels for candidate (have: '(_:)', expected: '(bar:)')}}
  func qadding(foo: Foo) -> MyView  { fatalError() } // expected-note{{incorrect labels for candidate (have: '(_:)', expected: '(foo:)')}}
}

func testCase() {
  let array: [Int]? = []

  makeView() {
    if array?.isEmpty == false {
      MyView().qadding(.foo) // expected-error{{no exact matches in call to instance method 'qadding'}}
    }
  }
 }