// RUN: %target-typecheck-verify-swift

public protocol P: Copyable {} // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
public struct S: P {}

public typealias PleaseLetMeDoIt = Copyable // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
public typealias WhatIfIQualify = Swift.Copyable // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}

public class C: Swift.Copyable {} // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}

public func whatever<T>(_ t: T) where T: Copyable {} // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
public func vatever<T: Copyable>(_ t: T) {} // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
public func alias<T: PleaseLetMeDoIt>(_ t: T) {}
public func buttever(_ t: any Copyable) {} // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
public func zuttever(_ t: some Copyable) {} // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}

public enum RockNRoll<T: Copyable> { // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
  case isNoisePollution(Copyable) // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
  case isMusic(T)
}

public enum namespace {
  typealias Copyable = Int

  func Copyable() -> Copyable { return 0 }
}

struct AVX: Copyable, ~Copyable, Sendable { // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
                                            // expected-error@-1 {{move-only struct 'AVX' cannot conform to 'Copyable'}}
  deinit {}
}

extension Copyable { // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
  func clone() -> Self { return self }
}

func checkIt(_ t: Copyable) // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
    -> Copyable { // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
  let y: Copyable = t // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
  return y
}
