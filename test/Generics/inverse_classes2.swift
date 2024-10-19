// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

class KlassModern: ~Copyable {} // expected-error {{classes cannot be '~Copyable'}}

actor FamousPerson: ~Copyable {} // expected-error{{actors cannot be '~Copyable'}}

class Konditional<T: ~Copyable> {}

func checks<T: ~Copyable, C>(
          _ b: KlassModern,
          _ c: Konditional<T>,
          _ d: Konditional<C>) {}
