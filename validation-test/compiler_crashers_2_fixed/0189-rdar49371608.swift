// RUN: %target-typecheck-verify-swift

struct A<T> {
  let foo: [T]
}

extension A : Codable where T: Codable {
  enum CodingKeys: String, CodingKey {
    case foo = "foo"
  }
}
