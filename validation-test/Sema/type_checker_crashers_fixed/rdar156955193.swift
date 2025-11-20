// RUN: %target-typecheck-verify-swift

protocol Definition {
  associatedtype Delegate
}

enum Kind {
  case normal
  case other
}

struct Payload {
}

extension Definition where Delegate == ((Kind, Payload) -> Void) {
  static func invokeDelegate(_ delegate: Delegate, kind: Kind, payload: Payload) {
    delegate(kind, payload)
  }
}

extension Definition where Delegate == ((Kind, Payload) -> Void)? {
  static func invokeOptionalDelegate(_ delegate: Delegate, kind: Kind, payload: Payload) {
    delegate?(kind, payload)
  }
}
