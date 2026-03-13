// RUN: %target-swift-frontend %s -emit-ir -sil-verify-all -o /dev/null

// Simply checks that the compiler doesn't crash.

// extra coverage
typealias ID = String
typealias Version = Int
func namedTupleExample(_ collection: [(ID, Version)?]) -> [(remoteID: ID, version: Version)?] {
  return collection
}


// example 1
protocol Kitty {}

public enum DictKey: String, Hashable, CaseIterable {
  case red, white, green
}

public final class FunctionTypeExample<Type> {
  typealias SourceFunc = ((_ kitty: Kitty) -> Type)
  typealias TargetFunc = ((_ kitty: Kitty) -> Type?)

  typealias Source = [DictKey: SourceFunc]
  typealias Target = [DictKey: TargetFunc]

  private var dict: Target = [:]

  init(dict: Source) {
    self.dict = dict
  }
}



// example 2
struct X {
  init?(_ values: some Collection<Any?>) { }

  static func get<T>(_ dict: [String: [T]]) -> X {
    let values = dict["hello"]!
    guard let thing = Self(values) else {
      fatalError("oops")
    }
    return thing
  }
}
