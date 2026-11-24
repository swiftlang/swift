// rdar://190321924
// RUN: %target-typecheck-verify-swift

enum Yes {}
enum No {}
struct Box<T> {}

func pick(_ cond: Bool) -> Any.Type {
  func open<T>(_: T.Type) -> Any.Type { Box<T>.self }
  return _openExistential(cond ? Yes.self : No.self, do: open) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
