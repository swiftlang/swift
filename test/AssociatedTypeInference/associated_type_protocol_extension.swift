// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype Body: P
  var body: Body { get }
}

// The S: P conformance should pick up 'var body'
// from 'extension PP'.
struct S<T> {}
extension S : P, PP where T : P {}

protocol PP : P {}
extension PP {
  var body: Never { fatalError() }
}

extension Never : PP {}
