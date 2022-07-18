// RUN: %swift-ide-test --code-completion --code-completion-token COMPLETE --source-filename %s
// Should not crash

protocol View2 {}

struct Foo {
    init(closure: () -> Void) {}
    func pnReceive(perform action: (MyResult) -> Void) -> some View2 {
        fatalError()
    }
}

struct SomeStruct {
    var string: String
}

@resultBuilder public struct ViewBuilder2 {
  public static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
}

@ViewBuilder2 var body: some View2 {
    Foo {}.pnReceive() { (value) in
        switch value {
        case let .success(#^COMPLETE^#raw, pretty):
            break
        }
    }
}

enum MyResult {
    case success(SomeStruct)
}
