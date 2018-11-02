// RUN: %target-typecheck-verify-swift -swift-version 3

enum Runcible {
  case spoon
  case hat
  @_downgrade_exhaustivity_check
  case fork
}

enum Trioptional {
  case some(Runcible)
  case just(Runcible)
  case none
}

enum Fungible {
  case cash
  case giftCard
}

func missingCases(x: Runcible?, y: Runcible?, z: Trioptional) {
  // Should warn in S3 mode that `fork` isn't used
  switch x! { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '.fork'}}
  case .spoon:
    break
  case .hat:
    break
  }

  // Should warn in S3 mode that `fork` isn't used
  switch (x!, y!) { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 2 {{add missing case:}}
  case (.spoon, .spoon):
    break
  case (.spoon, .hat):
    break
  case (.hat, .spoon):
    break
  case (.hat, .hat):
    break
  }
  
  // Should error, since `fork` is used but not totally covered
  switch (x!, y!) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.fork, .hat)'}}
  // expected-note@-2 {{add missing case: '(_, .fork)'}}
  case (.spoon, .spoon):
    break
  case (.spoon, .hat):
    break
  case (.hat, .spoon):
    break
  case (.hat, .hat):
    break
  case (.fork, .spoon):
    break
  }
  
  // Should error, since `fork` is used but not totally covered
  switch (x!, y!) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.fork, _)'}}
  // expected-note@-2 {{add missing case: '(.spoon, .fork)'}}
  case (.spoon, .spoon):
    break
  case (.spoon, .hat):
    break
  case (.hat, .spoon):
    break
  case (.hat, .hat):
    break
  case (.hat, .fork):
    break
  }
  
  // Should warn in S3 mode that `fork` isn't used
  switch x { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '.some(.fork)'}}
  case .some(.spoon):
    break
  case .some(.hat):
    break
  case .none:
    break
  }
  
  // Should warn in S3 mode that `fork` isn't used
  switch (x, y!) { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.some(.fork), _)'}}
  // expected-note@-2 {{add missing case: '(_, .fork)'}}
  case (.some(.spoon), .spoon):
    break
  case (.some(.spoon), .hat):
    break
  case (.some(.hat), .spoon):
    break
  case (.some(.hat), .hat):
    break
  case (.none, .spoon):
    break
  case (.none, .hat):
    break
  }

  // Should warn in S3 mode that `fork` isn't used
  switch (x, y) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.some(.fork), _)'}}
  // expected-note@-2 {{add missing case: '(_, .some(.fork))'}}
  // expected-note@-3 {{add missing case: '(.none, _)'}}
  case (.some(.spoon), .some(.spoon)):
    break
  case (.some(.spoon), .some(.hat)):
    break
  case (.some(.spoon), .none):
    break
  case (.some(.hat), .some(.spoon)):
    break
  case (.some(.hat), .some(.hat)):
    break
  case (.some(.hat), .none):
    break
  case (.some(.hat), .some(.spoon)): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    break
  case (.some(.hat), .some(.hat)): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    break
  case (.some(.hat), .none): // expected-warning {{case is already handled by previous patterns; consider removing it}}
    break
  }
  
  // Should warn in S3 mode that `fork` isn't used
  switch z { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '.some(.fork)'}}
  // expected-note@-2 {{add missing case: '.just(.fork)'}}
  case .some(.spoon):
    break
  case .some(.hat):
    break
  case .just(.spoon):
    break
  case .just(.hat):
    break
  case .none:
    break
  }
}
