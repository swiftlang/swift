// RUN: %target-swift-emit-ir %s -module-name=main -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu

@propertyWrapper
@dynamicMemberLookup
public struct Binding<Value> {
  public var wrappedValue: Value

  init(get: @escaping () -> Value, set: @escaping (Value) -> Void) {
    self.wrappedValue = get()
  }

  subscript<Subject>(dynamicMember keyPath: WritableKeyPath<Value, Subject>) -> Binding<Subject> {
    get { fatalError() }
  }
}


public struct State<Wrapped> {
  public var wrappedValue: Wrapped
  
  public init(wrappedValue: Wrapped) {
    self.wrappedValue = wrappedValue
  }
  public var projectedValue: Projection {
    Projection(wrappedValue: wrappedValue)
  }

  @dynamicMemberLookup
  public struct Projection {
    var wrappedValue: Wrapped
    public subscript<Member>(dynamicMember keyPath: ReferenceWritableKeyPath<Wrapped, Member>) -> Binding<Member> where Wrapped: AnyObject {
      Binding(get: {
        wrappedValue[keyPath: keyPath]
      }, set: { newValue in
        wrappedValue[keyPath: keyPath] = newValue
      })
    }
  }
}

// CHECK: define {{.*}}@main(
