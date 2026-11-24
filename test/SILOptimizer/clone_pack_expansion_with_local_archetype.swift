// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/94501
// AllocBoxToStack clones closures that capture boxes. Cloning a substitution
// map which contains a local archetype must not change pack expansions of
// primary pack archetypes, e.g. `Pack{repeat each Value}` must not become
// `Pack{repeat (each Value)^1}`. This caused a crash in GenericSpecializer.

// CHECK-NOT: level:

enum E: Error {
  case x
}

struct Arg<Value> {
  let convert: (Int) -> Value?
}

func args<each Value>(_ args: repeat Arg<each Value>) -> Result<(repeat each Value), Error> {
  var remaining = [1]

  func matchNext<Matched>(_ input: Arg<Matched>) throws -> Matched {
    if let current = remaining.first, let match = input.convert(current) {
      remaining.removeFirst()
      return match
    }
    throw E.x
  }

  func matchAll() throws -> (repeat each Value) {
    let matched = (repeat try matchNext(each args))
    guard remaining.isEmpty else {
      throw E.x
    }
    return matched
  }

  do {
    return .success(try matchAll())
  } catch {
    return .failure(error)
  }
}

public func test() -> String? {
  let result: Result<(String), Error> = args(Arg(convert: { _ in "hello" }))
  return try? result.get()
}

// Same as above, but with an additional scalar generic parameter which is
// used together with the pack in the nested functions' substitution maps.
func args2<T, each Value>(_ t: T, _ args: repeat Arg<each Value>) -> Result<(T, repeat each Value), Error> {
  var remaining = [1]

  func matchNext<Matched>(_ input: Arg<Matched>) throws -> (Matched, T) {
    if let current = remaining.first, let match = input.convert(current) {
      remaining.removeFirst()
      return (match, t)
    }
    throw E.x
  }

  func matchAll() throws -> (T, repeat each Value) {
    let matched = (repeat try matchNext(each args).0)
    guard remaining.isEmpty else {
      throw E.x
    }
    return (t, repeat each matched)
  }

  do {
    return .success(try matchAll())
  } catch {
    return .failure(error)
  }
}

public func test2() -> (Int, String)? {
  let result: Result<(Int, String), Error> = args2(27, Arg(convert: { _ in "hello" }))
  return try? result.get()
}
