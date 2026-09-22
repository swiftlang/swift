// RUN: %target-swift-emit-ir %s -wmo
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo

// A heap allocation in a function of another module - e.g. in a standard library function or in a
// specialization of one - is reported at the innermost call in this file, because that's the code
// which can be changed. If a violation is not reached through a call from this file - e.g. because
// the containing function is only referenced from a vtable - it can only be reported in the
// standard library itself. '-verify-ignore-unrelated' ignores those.

// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -Werror HeapAllocation -wmo -verify -verify-ignore-unknown -verify-ignore-unrelated

// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -no-allocations -wmo -verify -verify-ignore-unknown -verify-ignore-unrelated

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public class X {} // expected-error {{creating an instance of type 'X' involves heap allocation}}
public func use_a_class() -> X {
	let x = X() // expected-note {{instance of type created here}}
	return x
}

public func use_an_array() -> Int {
	let a = [1, 2, 3] // expected-error {{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}
	return a.count
}

public func use_unsafepointer_allocate() -> UnsafeMutablePointer<UInt8> {
	return UnsafeMutablePointer<UInt8>.allocate(capacity: 10) // expected-error {{explicit heap allocation}}
}

func acceptEscaping(_ body: @escaping () -> Void) { }

public func passEscaping(i: Int) {

  acceptEscaping {
    print(17)
  }

  acceptEscaping { // expected-error{{escaping closure involves heap allocation}}
    print(i)
  }
}

public enum SyntaxTree {
  case integerLiteral(Int)
  case variable(String)
  indirect case add(SyntaxTree, SyntaxTree)
}

public func getVariable(_ name: String) -> SyntaxTree {
  return .variable(name)
}

public func addEm(lhs: SyntaxTree, rhs: SyntaxTree) -> SyntaxTree {
  // TODO: this diagnostic could be better
  return .add(lhs, rhs) // expected-error{{creating an instance of type '{ var (SyntaxTree, SyntaxTree) }' involves heap allocation}}
}

public protocol P { }

public enum HomeworkError: Error, P {
  case forgot
  case dogAteIt(String)
}

public func getHomeworkError(dogName: String?) -> HomeworkError {
  if let dogName {
    return .dogAteIt(dogName)
  }

  return .forgot
}

public func getHomeworkErrorAsAnyError(dogName: String) -> any Error {
  return HomeworkError.dogAteIt(dogName) // expected-error{{boxing a value of type 'HomeworkError' into an 'any Error' involves heap allocation}}
}

public struct BigType: P {
  var values: (Double, Double, Double, Double, Double, Double, Double, Double)
}

public func getExistentialPSmall() -> any P {
  return HomeworkError.forgot
}

public func getExistentialErrorPSmall() -> any P & Error {
  return HomeworkError.forgot
}

public func getExistentialPBig() -> any P {
  return BigType(values: (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)) // expected-error{{boxing a value of type 'BigType' into an 'any P' involves heap allocation}}
}

public func getExistentialMetaPSmall() -> any (P.Type) {
  return HomeworkError.self
}

public func getExistentialMetaPBig() -> any (P.Type) {
  // TODO: can we drop the @thick somehow?
  return BigType.self // expected-error{{boxing type 'BigType' into an '@thick any P.Type' can result in later heap allocation}}
}

// TODO: async functions require _Concurrency, which brings in some
// allocation. Test for await calls later.
