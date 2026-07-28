// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// https://github.com/swiftlang/swift/issues/83826
//
// `do throws(T) { try inner() }` where `inner` throws a different type that's
// implicitly convertible to T must run the appropriate conversion in SILGen
// rather than asserting that the destination is `any Error`. Two concrete
// shapes flow through the same `emitThrow` site:
//
//   1. The inner thrown type is concrete and `T` is `any P` for some protocol
//      `P` that refines `Error` — needs existential erasure to `any P` (not
//      `any Error`), with a conformance to `P`.
//   2. The inner thrown type is a subclass of `T` — needs an `upcast`.

protocol AbstractError: Error {
  var message: String { get }
}

struct ConcreteError: AbstractError {
  let message: String
}

func throwsConcrete() throws(ConcreteError) {
  throw ConcreteError(message: "from concrete")
}

func testProtocolErasure() {
  do throws(any AbstractError) {
    try throwsConcrete()
  } catch {
    print("erased:", error.message)
  }
}
testProtocolErasure()
// CHECK: erased: from concrete

class BaseError: Error, CustomStringConvertible {
  var description: String { "base" }
}
class SubError: BaseError {
  override var description: String { "sub" }
}

func throwsSub() throws(SubError) {
  throw SubError()
}

func testClassUpcast() {
  do throws(BaseError) {
    try throwsSub()
  } catch {
    print("upcast:", error)
  }
}
testClassUpcast()
// CHECK: upcast: sub

// Composed existential — destination is `any (P & Q)`. Both protocol
// conformances must be looked up, not just `Error`.
protocol Tag {}
struct ConcreteTagged: AbstractError, Tag {
  let message: String
}
func throwsTagged() throws(ConcreteTagged) {
  throw ConcreteTagged(message: "from tagged")
}
func testComposedExistential() {
  do throws(any AbstractError & Tag) {
    try throwsTagged()
  } catch {
    print("composed:", error.message)
  }
}
testComposedExistential()
// CHECK: composed: from tagged

// Marker-protocol composition: `Sendable` is a marker protocol so its
// conformance lookup produces an abstract conformance. The existential
// erasure should still complete cleanly.
struct SendableConcreteError: Error, Sendable {
  var description: String { "sendable" }
}
func throwsSendable() throws(SendableConcreteError) {
  throw SendableConcreteError()
}
func testMarkerComposition() {
  do throws(any Error & Sendable) {
    try throwsSendable()
  } catch {
    print("marker:", type(of: error))
  }
}
testMarkerComposition()
// CHECK: marker: SendableConcreteError
