// RUN: not %target-swift-frontend %s -typecheck

protocol A {}

class C<T> where T: A {}

extension C {
  func foo() {
    extension C where T: Undefined {
      class Inner: Encodable {
        var foo: Int
      }
    }
  }
}
