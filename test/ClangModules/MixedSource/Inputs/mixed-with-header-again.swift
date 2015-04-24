@exported import MixedWithHeader

public func testLineImpl(line: Line) {
  doSomethingElse(line)
}

public func testOriginal(a: ForwardClass, _ b: Base, _ c: ProtoConformer) {}
