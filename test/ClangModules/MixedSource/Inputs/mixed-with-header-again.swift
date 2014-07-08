@exported import MixedWithHeader

public func testLineImpl(line: Line) {
  doSomethingElse(line)
}

public func testOriginal(a: ForwardClass, b: Base, c: ProtoConformer) {}
