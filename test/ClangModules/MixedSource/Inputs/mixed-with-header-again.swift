@_exported import MixedWithHeader

public func testLineImpl(_ line: Line) {
  doSomethingElse(line)
}

public func testOriginal(_ a: ForwardClass, _ b: Base, _ c: ProtoConformer) {}
