// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -verify
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -enable-strings

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu

public func test1() {
  print("x") // OK
}

public func test2(_ s: String) {  // expected-error {{String has large associated codesize cost; only available in embedded Swift by explicitly passing -enable-strings}}
}

public func test3() {
  let _ = "abc" // expected-error {{String has large associated codesize cost; only available in embedded Swift by explicitly passing -enable-strings}}
}

public func test4() {
  let _ = "abc \(42)" // expected-error {{String has large associated codesize cost; only available in embedded Swift by explicitly passing -enable-strings}}
  // expected-error@-1 {{String has large associated codesize cost; only available in embedded Swift by explicitly passing -enable-strings}}
  // expected-error@-2 {{String has large associated codesize cost; only available in embedded Swift by explicitly passing -enable-strings}}
}

@_unavailableInEmbedded
public func test5() {
  let _ = "abc" // OK
}
