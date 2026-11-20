// rdar://171026861
// RUN: %target-typecheck-verify-swift

protocol Writer {
  associatedtype Element
}

extension Writer {
  func write(_ element: Element) {}
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
  server.serve { writer in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    writer.write("Hello".utf8Span)
  }
}
