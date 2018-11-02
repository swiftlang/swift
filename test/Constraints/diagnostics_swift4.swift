// RUN: %target-typecheck-verify-swift -swift-version 4

// SR-2505: "Call arguments did not match up" assertion

func sr_2505(_ a: Any) {} // expected-note {{}}
sr_2505()          // expected-error {{missing argument for parameter #1 in call}}
sr_2505(a: 1)      // expected-error {{extraneous argument label 'a:' in call}}
sr_2505(1, 2)      // expected-error {{extra argument in call}}
sr_2505(a: 1, 2)   // expected-error {{extra argument 'a' in call}}

struct C_2505 {
  init(_ arg: Any) {
  }
}

protocol P_2505 {
}

extension C_2505 {
  init<T>(from: [T]) where T: P_2505 {
  }
}

class C2_2505: P_2505 {
}

let c_2505 = C_2505(arg: [C2_2505()]) // expected-error {{argument labels '(arg:)' do not match any available overloads}}
// expected-note@-1 {{overloads for 'C_2505' exist with these partially matching parameter lists: (Any), (from: [T])}}

// rdar://problem/31898542 - Swift 4: 'type of expression is ambiguous without more context' errors, without a fixit

enum R31898542<T> {
  case success(T) // expected-note {{'success' declared here}}
  case failure
}

func foo() -> R31898542<()> {
  return .success() // expected-error {{missing argument for parameter #1 in call}} {{19-19=<#T#>}}
}

// rdar://problem/31973368 - Cannot convert value of type '(K, V) -> ()' to expected argument type '((key: _, value: _)) -> Void'

// SE-0110: We reverted to allowing this for the time being, but this
// test is valuable in case we end up disallowing it again in the
// future.

class R<K: Hashable, V> {
  func forEach(_ body: (K, V) -> ()) {
    let dict: [K:V] = [:]
    dict.forEach(body)
  }
}

// Make sure that solver doesn't try to form solutions with available overloads when better generic choices are present.
infix operator +=+ : AdditionPrecedence
func +=+(_ lhs: Int, _ rhs: Int) -> Bool { return lhs == rhs }
func +=+<T: BinaryInteger>(_ lhs: T, _ rhs: Int) -> Bool { return lhs == rhs }
// FIXME: DoubleWidth is no longer part of the standard library
// let _ = DoubleWidth<Int>(Int.min) - 1 +=+ Int.min // Ok
