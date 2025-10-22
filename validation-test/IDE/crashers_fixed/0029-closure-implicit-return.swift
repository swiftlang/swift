// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

public enum MyEnum {
  case one
  case two
}

struct MyStruct {
  func takesClosure(_ action: (() -> Void)) -> MyStruct {
    return self
  }

  func generic<V>(_ keyPath: V, _ value: MyEnum) -> MyStruct {
    return self
  }
}

@resultBuilder struct MyBuilder {
  static func buildBlock(_ content: MyStruct) -> MyStruct {
    return content
  }
}

struct ItemDetailView {
    @MyBuilder var body: MyStruct {
        MyStruct()
        .generic("xorizontalSizeClass", .#^COMPLETE^#regular)
        .takesClosure {
            "abc"
        }
    }
}
