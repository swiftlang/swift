// RUN: %target-typecheck-verify-swift

// Make sure that the source range of a protocol requirement
// with no body still includes the thrown type, by checking
// that the name lookup from there finds the generic parameter.

protocol TypedThrowsProto {
    init<E>(y: () throws(E) -> Void) throws(E)
    func f<E>(y: () throws(E) -> Void) throws(E)
}
