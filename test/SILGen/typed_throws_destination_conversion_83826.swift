// FIXME: crashes under opaque values
// RUN: not --crash %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -module-name main %s

// RUN: %target-swift-emit-silgen -module-name main %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/83826
//
// `do throws(T) { try inner() }` where `inner` throws a different (compatible)
// type lowers to a conversion before branching to the catch's error block.
// Lock down the SIL shape so future refactors don't accidentally lose the
// erasure / upcast.

protocol AbstractError: Error {
    var message: String { get }
}

struct ConcreteError: AbstractError {
    let message: String
}

func throwsConcrete() throws(ConcreteError) {
    throw ConcreteError(message: "x")
}

// CHECK-LABEL: sil hidden [ossa] @$s4main19testProtocolErasureyyF
// `do throws(any AbstractError) { try throwsConcrete() }` should erase the
// concrete struct to the destination existential. The alloc_stack is for
// `any AbstractError` (NOT `any Error`), and the init_existential_addr
// places the in-flight $ConcreteError into it.
// CHECK: alloc_stack $any AbstractError
// CHECK: init_existential_addr {{.*}}, $ConcreteError
// CHECK-NOT: alloc_stack $any Error
// CHECK: end sil function '$s4main19testProtocolErasureyyF'
func testProtocolErasure() {
    do throws(any AbstractError) {
        try throwsConcrete()
    } catch {
        _ = error.message
    }
}

class BaseError: Error {}
class SubError: BaseError {}

func throwsSub() throws(SubError) {
    throw SubError()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main15testClassUpcastyyF
// `do throws(BaseError) { try throwsSub() }` should upcast the in-flight
// $SubError to $BaseError before branching to the catch's error block.
// The class path must NOT route through existential erasure.
// CHECK: upcast {{.*}} to $BaseError
// CHECK-NOT: init_existential_addr
// CHECK-NOT: alloc_existential_box
// CHECK: end sil function '$s4main15testClassUpcastyyF'
func testClassUpcast() {
    do throws(BaseError) {
        try throwsSub()
    } catch {
        _ = error
    }
}

// Composed existential: `any (P & Q)` requires conformances to both protocols.
// The erasure should look up both, not just one.
protocol Tag {}
struct ConcreteTagged: AbstractError, Tag {
    let message: String
}

func throwsTagged() throws(ConcreteTagged) {
    throw ConcreteTagged(message: "y")
}

// CHECK-LABEL: sil hidden [ossa] @$s4main23testComposedExistentialyyF
// CHECK: alloc_stack $any AbstractError & Tag
// CHECK: init_existential_addr {{.*}}, $ConcreteTagged
// CHECK-NOT: alloc_stack $any Error
// CHECK: end sil function '$s4main23testComposedExistentialyyF'
func testComposedExistential() {
    do throws(any AbstractError & Tag) {
        try throwsTagged()
    } catch {
        _ = error.message
    }
}

// `do throws(any Error)` (the path that was previously the only one allowed)
// must still produce existential erasure to `any Error`.
struct PlainError: Error {}
func throwsPlain() throws(PlainError) { throw PlainError() }

// CHECK-LABEL: sil hidden [ossa] @$s4main12testAnyErroryyF
// `any Error` uses the boxed-error representation rather than a generic
// existential container; verify the box machinery still kicks in via the
// new dispatch and that we don't accidentally switch to the generic
// existential path.
// CHECK: alloc_existential_box $any Error, $PlainError
// CHECK-NOT: init_existential_addr {{.*}}, $PlainError
// CHECK: end sil function '$s4main12testAnyErroryyF'
func testAnyError() {
    do throws(any Error) {
        try throwsPlain()
    } catch {
        _ = error
    }
}
