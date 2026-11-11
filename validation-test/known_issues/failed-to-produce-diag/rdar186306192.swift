// rdar://186306192
// RUN: %target-typecheck-verify-swift

struct FileType: Hashable {}

func small(_: [String: FileType]?) {}

func test(d: [String: String], b: Bool) {
  small(b ? nil : d)
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
