// rdar://107143408
// RUN: %target-typecheck-verify-swift

protocol View {}

struct Text: View {
  init(_: String) {}
}

@resultBuilder
struct ViewBuilder {
  static func buildBlock<C: View>(_ c: C) -> C { c }
}

struct ScrollView<Content: View>: View {
  init(@ViewBuilder content: () -> Content) {}
}

struct DictView: View {
  init(dictionary: [String: Any]) {}
}

class HTTPURLResponse {
  var allHeaderFields: [AnyHashable: Any] { [:] }
}

func test(response: HTTPURLResponse?) -> some View {
  ScrollView { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    DictView(dictionary: response?.allHeaderFields ?? [:])
  }
}

func takesDict(_: [String: Any]) {}

struct Test {
  let dict: [AnyHashable: Any] = [:]
}

func test(t: Test?) {
  _ = { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    takesDict(t?.dict ?? [:])
  }
}
