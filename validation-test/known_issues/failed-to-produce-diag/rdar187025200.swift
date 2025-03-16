// rdar://187025200
// RUN: %target-typecheck-verify-swift

class Connection {}

class Request {
  func accept<M: Decodable>(handler: @escaping (M) -> (any Encodable)?) -> Int { 0 }
  func accept<M: Decodable>(handler: @escaping (M) -> (any Encodable)?) -> (Int, Int) { (0, 0) }
}

struct Handler {
  init(onConnection: @escaping (Connection) -> Bool) {}
}

struct Reply: Codable {}

func test() -> Handler {
  Handler { (request: Request) in // expected-error {{function value passed to 'init' as argument #1 could be any of '(Request) -> (Int, Int)', '(Request) -> Int' but '(Connection) -> Bool' is expected}}
    request.accept { (_: Int) -> Reply? in
      Reply()
    }
  }
}
