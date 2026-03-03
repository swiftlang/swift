// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

enum MyEnum {
    case foo
}

func use(_ value: Any?, _ policy: MyEnum) {}

struct Tester<Outer> {
  func test<Inner>(_ value: Inner?) {
    use(value, .#^COMPLETE^#foo)
  }
}
