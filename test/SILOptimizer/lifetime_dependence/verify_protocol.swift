// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -sil-verify-all \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// rdar://189111544 (Non-escapable result of a protocol extension method can't outlive its declaration when the
// receiver is a temporary)

public protocol Owning {
  var value: Int { get }
}

public final class Owner: Owning {
  public init() {}
  public var value = 1
}

public struct Dependent: ~Escapable {
  let box: Owner

  @_lifetime(immortal)
  init(box: Owner) { self.box = box }

  public var value: Int { box.value }
}

extension Owning {
  public var dependentProperty: Dependent {
    @_lifetime(borrow self)
    get { _overrideLifetime(Dependent(box: Owner()), borrowing: self) }
  }

  @_lifetime(borrow self)
  public func dependentMethod() -> Dependent {
    _overrideLifetime(Dependent(box: Owner()), borrowing: self)
  }
}

func run(_ body: () -> Void) { body() }

func makeOwner() -> Owner { Owner() }

struct Client {
  let storedOwner: Owner
  var computedOwner: Owner { storedOwner }

  func propertyThroughComputed() {
    let d = computedOwner.dependentProperty
    run { print(d.value) }
  }

  func methodThroughStored() {
    let d = storedOwner.dependentMethod()
    run { print(d.value) }
  }

  func methodThroughComputed() {
    let d = computedOwner.dependentMethod()
    run { print(d.value) }
  }

  func methodThroughComputedDirectUse() {
    let d = computedOwner.dependentMethod()
    print(d.value)
    print(d.value)
  }
}

func methodThroughCallResult() -> Int {
  let d = makeOwner().dependentMethod()
  return d.value
}

@_lifetime(immortal)
func methodThroughTemporaryEscapes() -> Dependent {
  makeOwner().dependentMethod() // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1{{it depends on the lifetime of this parent value}}
} // expected-note {{this use causes the lifetime-dependent value to escape}}
