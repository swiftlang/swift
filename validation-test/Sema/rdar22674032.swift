// rdar://22674032
// RUN: %target-typecheck-verify-swift

struct Keys {
  static let key = "key"
}

let dictionary = [Keys.key: "val", Keys.Type: "val"] // expected-error {{cannot convert value of type 'Keys.Type.Type' to expected dictionary key type 'AnyHashable'}}
