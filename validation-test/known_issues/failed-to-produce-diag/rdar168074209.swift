// rdar://168074209
// RUN: %target-typecheck-verify-swift

enum Status {
  case failure(title: String, subtitle: String? = nil, retryAction: () -> Void)
}

struct StatusView {
  let state: Status
}

func test(title: String, handler: (() -> Void)?) {
  _ = StatusView(state: .failure(title: title, subtitle: nil, retryAction: handler?())) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
