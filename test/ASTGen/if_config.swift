// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen -DDISCARDABLE -DNONSENDABLE -swift-version 6

// REQUIRES: swift_feature_ParserASTGen

#if NOT_SET
func f { } // expected-error{{expected parameter clause in function signature}}
           // expected-note@-1{{insert parameter clause}}{{7-8=}}{{8-8=(}}{{8-8=) }}
#endif

#if compiler(>=10.0)
bad code is not diagnosed
#endif

#if canImport(NoSuchModule)
func g() { }
#else
func h() { }
#endif


func testH() {
  h()
}

// Declaration attributes
#if DISCARDABLE
@discardableResult
#endif
func ignorable() -> Int { 5 }


func testIgnorable() {
  ignorable() // no warning
}

// Type attributes
// FIXME: Something isn't getting converted properly when I try to put an #if
// within the type attributes.
typealias CallbackType = () -> Void

func acceptMaybeSendable(body: CallbackType) { }

func testMaybeSendable() {
  var x = 10
  acceptMaybeSendable {
    x += 1
  }
  print(x)
}

// Switch cases
enum SafetyError: Error {
case unsafe

#if NONSENDABLE
case nonSendable
#endif
}

extension SafetyError {
  var description: String {
    switch self {
    case .unsafe: "unsafe"

#if NONSENDABLE
    case .nonSendable: "non-sendable"
#endif
    }
  }
}
