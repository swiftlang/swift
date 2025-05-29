// RUN: %batch-code-completion

protocol MyProtocol {}
struct MyStruct: MyProtocol {
  func aspectRatio(contentMode: Int) -> MyStruct { fatalError() }
}

func foo<I>(content: (MyStruct) -> I) where  I : MyProtocol { fatalError() }

func bazx() {
  foo() { image in
    image.aspectRatio(#^COMPLETE^#)
  }
}

// COMPLETE: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]/TypeRelation[Convertible]: ['(']{#contentMode: Int#}[')'][#MyStruct#]; name=contentMode:
