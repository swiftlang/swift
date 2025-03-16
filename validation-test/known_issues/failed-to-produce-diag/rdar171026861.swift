// rdar://171026861
// RUN: %target-typecheck-verify-swift

protocol Writer {
  associatedtype Element
}

extension Writer {
  // expected-note@+1{{candidate expects value of type 'UInt8' for parameter #1 (got 'UTF8Span')}}
  func write(_ element: Element) {}
  // expected-note@+1{{candidate expects value of type 'Span<UInt8>' for parameter #1 (got 'UTF8Span')}}
  func write(_ span: Span<Element>) {}
}

protocol Handler<W> {
  associatedtype W: Writer
}

protocol Server {
  associatedtype W: Writer where W.Element == UInt8

  func serve(handler: some Handler<W>)
}

extension Server {
  func serve(handler: (W) -> Void) {}
}

@available(SwiftStdlib 6.2, *)
func test(_ server: some Server) {
  server.serve { writer in
    // expected-error@+1{{cannot convert argument #1 with type 'UTF8Span' to any of 'Span<UInt8>', 'UInt8' in call to 'write'}}
    writer.write("Hello".utf8Span)
  }
}
