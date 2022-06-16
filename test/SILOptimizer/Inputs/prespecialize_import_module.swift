public func someFunc<T>(_ t: T) {
  print(t)
}

@_specialize(exported: true, target: someFunc(_:), where T == Int)
@usableFromInline
func __specialize_someFunc<T>(_: T) {}
