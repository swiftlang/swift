// RUN: %swift-ide-test --code-completion --source-filename %s -code-completion-token CC

func foo() {
    let months: [[String]] = []
    let abcd: Int
    let x = ForEach2(months) { group in
        HStack2(spacing: 3) {
            useKeyPath(id: \.abcd#^CC^#)
        }
    }
}


struct ForEach2<Data> where Data : RandomAccessCollection {
    init(_ data: Data, content: @escaping (Data.Element) -> Void) {}
}

struct Bar {
	let abcd: Int
}

func useKeyPath(id: KeyPath<Bar, String>) {}

struct HStack2<Content> {
    init(spacing: Double, @ViewBuilder2 content: () -> Content)
}

@resultBuilder struct ViewBuilder2 {
  static func buildBlock<Content>(_ content: Content) -> Content { fatalError() }
}
