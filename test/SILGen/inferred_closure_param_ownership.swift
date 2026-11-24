// RUN: %target-swift-frontend -emit-sil -verify %s

// rdar://191403517
final class Store { var value = 0 }
struct Token: ~Copyable { let store: Store; var value: Int { store.value } }
func withToken(_ s: Store, _ body: (borrowing Token) -> Void) { body(Token(store: s)) }

func crashNested(_ s: Store) {
  // Ensure that we get an error about the nested capture by an escaping closure
  // rather than a crash.
  withToken(s) { t in let f = { t.value }; _ = f() } // expected-error{{'t' cannot be captured}} expected-note{{capturing 't' here}}
}
