struct MyGenericStruct<T> {
    lazy var prop2 = [AnotherGenericStruct<T>]()
}
struct AnotherGenericStruct<T> { }
