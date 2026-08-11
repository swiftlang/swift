// RUN: %target-swift-emit-ir %s -wmo
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/ArrayShared.swift':48{{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}" >> %t/main.swift
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/SwiftNativeNSArray.swift':501{{creating an instance of type '__SwiftNativeNSArrayWithContiguousStorage' involves heap allocation}}" >> %t/main.swift
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/ContiguousArrayBuffer.swift':361{{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}" >> %t/main.swift
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/UnsafePointer.swift':831{{explicit heap allocation}}" >> %t/main.swift

// RUN: %target-swift-emit-ir %t/main.swift -enable-experimental-feature Embedded -Werror HeapAllocation -wmo -verify -verify-ignore-unknown

// RUN: %target-swift-emit-ir %t/main.swift -enable-experimental-feature Embedded -no-allocations -wmo -verify -verify-ignore-unknown

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

//--- main.swift

public class X {} // expected-error {{creating an instance of type 'X' involves heap allocation}}
public func use_a_class() -> X {
	let x = X() // expected-note {{instance of type created here}}
	return x
}

public func use_an_array() -> Int {
	let a = [1, 2, 3] // expected-note*{{generic specialization called here}}
	return a.count
}

public func use_unsafepointer_allocate() -> UnsafeMutablePointer<UInt8> {
	return UnsafeMutablePointer<UInt8>.allocate(capacity: 10) // expected-note {{generic specialization called here}}
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
