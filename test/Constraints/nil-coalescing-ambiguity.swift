// RUN: %target-typecheck-verify-swift

// Expression that appeared in a real project.

struct URLComponents {
  var queryItems: [URLQueryItem]?
}

struct URLQueryItem {
  var name: String
  var value: String?
}

extension String {
  var removingPercentEncoding: String? { fatalError() }
}

func moreComplexExample1(_ urlComponents: URLComponents) -> [String: AnyHashable] {
  let result = urlComponents.queryItems?.reduce(into: [String: AnyHashable]()) { partialResult, queryItem in
      partialResult[queryItem.name] = queryItem.value?.removingPercentEncoding
  } ?? [:]

  // The type of result must be inferred as [String: AnyHashable], and not
  // [String: AnyHashable]?, [String: Any], or [String: Any]?, even though
  // the latter two solutions have a better score under our existing scoring
  // rules, which are hard to change. The trick is that we must not attempt
  // the 'Any' binding for the dictionary element type at all.
  return result
}

// Further reductions of the above.

do {
  func f<U, V>(_: U, _: V, _: (U, V) -> ()) -> U? { fatalError() }

  func g<V>(_: Array<V>, _: V) {}

  func test1a(u: [String], v: any Sequence) -> [any Sequence] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1b(u: [any Sequence], v: String) -> [any Sequence] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1c(u: [String], v: String?) -> [String?] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1d(u: [String?], v: String) -> [String?] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1e(u: [String], v: Any) -> [Any] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1f(u: [Any], v: String) -> [Any] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1g(u: [String], v: AnyHashable) -> [AnyHashable] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1h(u: [AnyHashable], v: String) -> [AnyHashable] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1i(u: [() -> Int], v: @escaping () -> Any) -> [() -> Any] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test1j(u: [() -> Any], v: @escaping () -> Int) -> [() -> Any] {
    let result = f(u, v) { u, v in g(u, v) } ?? []
    return result
  }

  func test2(u: [AnyHashable], v: String) -> [AnyHashable] {
    let result = f(u, v, g) ?? []
    return result
  }

  func test3(u: [AnyHashable], v: String) -> [AnyHashable] {
    let result = f([], v) { u, v in g(u, v) } ?? u
    return result
  }

  func test4(u: [AnyHashable], v: String) -> [AnyHashable] {
    let result = f([], v, g) ?? u
    return result
  }

  func test5(u: [AnyHashable], v: String) -> [AnyHashable] {
    let result = (f(u, v) { u, v in g(u, v) })!
    return result
  }

  func test6(u: [AnyHashable], v: String) -> [AnyHashable] {
    let result = f(u, v, g)!
    return result
  }
}

// Some regression tests reduced from projects.
do {
  struct S {}

  func f(x: S?) -> S {
    let y = x ?? {
      preconditionFailure("")
    }()
    return y
  }
}

do {
  func format(_ value: Double) -> String {
    return ""
  }

  func toKeyValList() -> [String: Any?] {
    return [:]
  }

  func toStr(verbose: Bool) -> String {
    return toKeyValList()
      .map { key, value in
        let text = (value as? Double).map(format)
                ?? (value as? Float).map { format(Double($0)) }
                ?? value.map { "\($0)" } ?? "-"
        return "\(key): \(text)"
      }.joined(separator: "")
  }
}

do {
  struct S {
    var name: String?
    var info: [Int: Any]?
    var value: String?
  }

  func f(s: S) -> String {
    let k = 0
    let x = s.name
                ?? s.info?[k] as? String
                ?? s.value.map { "\($0)" }
                ?? ""
    return x
  }
}

do {
  func f() -> String {
    let x: String? = nil
    let y: String? = nil
    let z: String? = nil

    let result = x ?? y ?? z.map { "\($0)" } ?? ""
    return result
  }
}

do {
  protocol P {}

  enum OneColumnGroup: P { case x }
  enum TwoColumnGroup: P { case x }
  enum ThreeColumnGroup: P { case x }

  struct S {
    var oneColumn: [OneColumnGroup]
    var twoColumnsCompact: [TwoColumnGroup]?
    var twoColumns: [TwoColumnGroup]?
    var threeColumn: [ThreeColumnGroup]?

    func f1() -> [any P] {
      return threeColumn ?? twoColumns ?? oneColumn
    }

    func f2() -> [any P] {
      return twoColumnsCompact ?? twoColumns ?? oneColumn
    }
  }
}