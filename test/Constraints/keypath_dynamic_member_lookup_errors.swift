// RUN: %target-typecheck-verify-swift

@dynamicMemberLookup
struct ObservedObject<T> {
  let root: T

  subscript<U>(
    dynamicMember keyPath: ReferenceWritableKeyPath<T, U>
  ) -> U {
    fatalError()
  }
}

protocol P {
  var checked: Bool { get set }
}

struct Test<U: P> {
  var v: ObservedObject<U>

  func test() {
    v.checked
    // expected-error@-1 {{referencing property 'checked' on 'U' requires it to be a class type}}
    // expected-note@-2 {{'subscript(dynamicMember:)' declared in 'ObservedObject<U>' uses 'ReferenceWritableKeyPath' which only supports classes}}
  }
}
