
#if TEST_LIBRARY_EVOLUTION
public struct MoveOnly : ~Copyable {
    var name = "John"
    public init() {}
    deinit {
        print("==> I am in the deinit resiliently!")
        print("==> My name is: \(name)!")
    }
}
#else
struct MoveOnly : ~Copyable {
    var name = "John"
    deinit {
        print("==> I am in the deinit!")
        print("==> My name is: \(name)!")
    }
}
#endif
