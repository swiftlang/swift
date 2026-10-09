// RUN: %target-typecheck-verify-swift -solver-scope-threshold=3600 -swift-version 5

class MyEntity {
  func asEntity(for: String?) throws -> (any AppEntity)? {
    fatalError()
  }
}

protocol AppEntity {}

protocol BigEntity {
  associatedtype Result: SmallEntity
  func shrink() throws -> Result
}

protocol SmallEntity: Item {}

extension SmallEntity {
    var cook: Snack {
        fatalError()
    }
}

struct Snack: Item {}

extension Array where Element == MyEntity {
    func slow<Entity: BigEntity>(
        type: Entity.Type,
        bundleIdentifier: String?,
        cooked: Bool
    ) -> any Item {
        return cooked ? compactMap {
            try? ($0.asEntity(for: bundleIdentifier) as? Entity)?
                .shrink()
                .cook
        } : compactMap {
            try? ($0.asEntity(for: bundleIdentifier) as? Entity)?
                .shrink()
        }
    }
}

protocol Item {}

extension Array: Item where Element: Item {}
