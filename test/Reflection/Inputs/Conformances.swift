public protocol MyProto {}
struct StructA : MyProto, Hashable {
    var x: Int
}
