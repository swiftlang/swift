// RUN: %target-swift-frontend -emit-sil -swift-version 5 -enable-library-evolution -primary-file %s -o /dev/null -verify

// Re-run with optimizations to check if -O does not make any difference
// RUN: %target-swift-frontend -O -emit-sil -swift-version 5 -enable-library-evolution -primary-file %s -o /dev/null -verify

public enum NonExhaustive {
  case a
}

@frozen public enum Exhaustive {
  case a
}

public var optional: Void? = nil

public func testNonExhaustive(_ e: NonExhaustive) {
  switch e {
  case .a: break
  }

  switch e {
  case .a: break
  default: break
  // expected-warning@-1 {{default will never be executed}}
  }

  switch e {
  case .a: break
  @unknown default: break
  // Ok, @unknown suppresses "default will never be executed"
  }

  switch (e, optional) {
  case (.a, _): break
  }

  switch (e, optional) {
  case (.a, _): break
  default: break
  // expected-warning@-1 {{default will never be executed}}
  }

  switch (e, optional) {
  case (.a, _): break
  @unknown default: break
  // Ok, @unknown suppresses "default will never be executed"
  }
}

@inlinable
public func testNonExhaustiveInlinable(_ e: NonExhaustive) {
  switch e {
  // expected-warning@-1 {{switch covers known cases, but 'NonExhaustive' may have additional unknown values}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}}
  case .a: break
  }

  switch e {
  case .a: break
  default: break
  }

  switch e {
  case .a: break
  @unknown default: break
  }

  switch (e, optional) {
  // expected-warning@-1 {{switch covers known cases, but '(NonExhaustive, Void?)' may have additional unknown values}}
  // expected-note@-2 {{add missing case: '(_, _)'}}
  case (.a, _): break
  }

  switch (e, optional) {
  case (.a, _): break
  default: break
  }

  switch (e, optional) {
  case (.a, _): break
  @unknown default: break
  }
}

public func testExhaustive(_ e: Exhaustive) {
  switch e {
  case .a: break
  }

  switch e {
  case .a: break
  default: break
  // expected-warning@-1 {{default will never be executed}}
  }

  switch e {
  case .a: break
  @unknown default: break
  // Ok, @unknown suppresses "default will never be executed"
  }

  switch (e, optional) {
  case (.a, _): break
  }

  switch (e, optional) {
  case (.a, _): break
  default: break
  // expected-warning@-1 {{default will never be executed}}
  }

  switch (e, optional) {
  case (.a, _): break
  @unknown default: break
  // Ok, @unknown suppresses "default will never be executed"
  }
}

@inlinable
public func testExhaustiveInlinable(_ e: Exhaustive) {
  switch e {
  case .a: break
  }

  switch e {
  case .a: break
  default: break
  // expected-warning@-1 {{default will never be executed}}
  }

  switch e {
  case .a: break
  @unknown default: break
  // Ok, @unknown suppresses "default will never be executed"
  }

  switch (e, optional) {
  case (.a, _): break
  }

  switch (e, optional) {
  case (.a, _): break
  default: break
  // expected-warning@-1 {{default will never be executed}}
  }

  switch (e, optional) {
  case (.a, _): break
  @unknown default: break
  // Ok, @unknown suppresses "default will never be executed"
  }
}
