protocol Publisher<Output> {
    associatedtype Output
}

extension Publisher {
  func stage2() -> MyGenerictype<Self.Output> {
    fatalError()
  }

  func stage3() -> Self {
    fatalError()
  }
}

struct MyGenerictype<Output> : Publisher {
  init() {}

  func stage1(with output: Self.Output) -> HasTypeAccessInTypealias<Self> {
    fatalError()
  }
}

struct HasTypeAccessInTypealias<Upstream> : Publisher where Upstream : Publisher {
  typealias Output = Upstream.Output
}

func test() {
  MyGenerictype()
    .stage1(with: 0)
    .stage2()
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):6 %s -- %s
    .stage3()
}

// CHECK: <Declaration>func stage3() -&gt; <Type usr="s:4test13MyGenerictypeV">MyGenerictype</Type>&lt;<Type usr="s:4test24HasTypeAccessInTypealiasV">HasTypeAccessInTypealias</Type>&lt;<Type usr="s:4test13MyGenerictypeV">MyGenerictype</Type>&lt;<Type usr="s:Si">Int</Type>&gt;&gt;.<Type usr="s:4test24HasTypeAccessInTypealiasV6Outputa">Output</Type>&gt;</Declaration>
