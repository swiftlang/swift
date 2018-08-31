// RUN: %target-typecheck-verify-swift

struct Box<Representation, T> {
    let value: Representation
}
enum Repr {}
extension Repr {
    typealias RawEnum = ()
}
extension Box where Representation == Repr, T == Representation.RawEnum {
    init(rawEnumValue: Representation.RawEnum) {
        let _: ().Type = T.self
        fatalError()
    }
}
