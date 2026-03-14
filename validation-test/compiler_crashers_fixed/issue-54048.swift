// RUN: %target-swift-frontend -primary-file %s -emit-silgen

// https://github.com/apple/swift/issues/54048

@propertyWrapper
struct MutatingGetNonMutatingSetWrapper<T> {
  private var fixed: T
  
  var wrappedValue: T {
    mutating get { fixed }
    nonmutating set { }
  }
  
  init(wrappedValue initialValue: T) {
    fixed = initialValue
  }
}

struct Foo {
  @MutatingGetNonMutatingSetWrapper var text: String
}
