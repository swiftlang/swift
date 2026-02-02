// RUN: %target-build-swift -O %s

@propertyWrapper
public struct Dependency2<Value> {
  public init(
    _ keyPath: KeyPath<DependencyValues, Value>
  ) {

  }
  public var wrappedValue: Value {
      let any: Any = "somestring"
      return any as! Value
  }
}

struct Client {
    var get: (_ id: String) -> Bool
}

public struct DependencyValues {
    var client3: Client
}

public protocol WindowData2 {
    var id :String {get set}
}

extension WindowData2 {
    func didSetProfileForSpaceIDs(
        spaceIDs: [String]
    ) -> Void {
        @Dependency2(\.client3) var client: Client
        spaceIDs.forEach { _ in
            spaceIDs
                .map { _ in return client.get(id) }
        }
    }
}
