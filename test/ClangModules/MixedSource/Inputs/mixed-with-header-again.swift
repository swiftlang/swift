@exported import MixedWithHeader

func testLineImpl(line: Line) {
  doSomethingElse(line)
}

func testOriginal(a: ForwardClass, b: Base, c: ProtoConformer) {}
