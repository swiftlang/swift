// rdar://181269544
// RUN: %target-typecheck-verify-swift

struct Data {}

protocol DecodableWithConfiguration {
  associatedtype DecodingConfiguration
}

class JSONDecoder {
  func decode<T: Decodable>(_ type: T.Type, from data: Data) throws -> T {
    fatalError()
  }
  func decode<T: DecodableWithConfiguration>(
    _ type: T.Type, from data: Data, configuration: T.DecodingConfiguration
  ) throws -> T {
    fatalError()
  }
}

func getUsers(data: Data) throws {
  let result = try JSONDecoder().decode(GitHubSearchResult.self, from: data)
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
  // expected-error@-2 {{cannot find 'GitHubSearchResult' in scope}}
}
