// rdar://22674032
// RUN: %target-typecheck-verify-swift

struct Keys {
  static let key = "key"
}

let dictionary = [Keys.key: "val", Keys.Type: "val"] // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
