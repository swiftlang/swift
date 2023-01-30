// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/50772

struct Box<Representation> {
    let value: Representation
}
enum Repr {}
extension Repr {
    typealias RawEnum = ()
}
extension Box where Representation == Repr {
    init(rawEnumValue: Representation.RawEnum) {
        fatalError()
    }
}
