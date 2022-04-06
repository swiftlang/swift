// RUN: %target-typecheck-verify-swift

protocol Schema: RawRepresentable {
  var rawValue: String { get }
}

enum MySchema: String, Schema {
  case hello = "hello"
}

struct Parser<S: Schema> :  Equatable {
}

protocol Entity {
}

protocol MyInitializable {
  init() throws
}

typealias MyInitializableEntity = MyInitializable & Entity

extension Parser where S == MySchema {
  func test(kind: Entity.Type) throws {
    guard let entityType = kind as? MyInitializableEntity.Type else {
      return
    }

    let _ = try entityType.init() // Ok
  }
}
