// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -verify %s
// RUN: %target-swift-frontend -dump-ast -parse-as-library %s | %sanitize-address > %t/library.ast
// RUN: %target-swift-frontend -dump-ast %s | %sanitize-address > %t/inferred.ast
// RUN: %diff -u %t/library.ast %t/inferred.ast

// Ensure that parsing this file in top-level code mode and then fixing up
// the AST is equivalent to parsing it in library mode in the first place.


func take(_ f: () -> Int) -> Int { f() }

@propertyWrapper
struct Wrap { var wrappedValue: Int }

@globalActor
actor GA { static let shared = GA() }

enum E { case a, b }

let ifExpr = if Bool.random() { take { 1 } } else { 2 }
let switchExpr = switch E.a {
case .a: take { 3 }
case .b: 4
}

let interpolated = "a\(1)b\(take { 4 })c"
let interpolatedInBranch = if Bool.random() { "d\(5)e" } else { "f" }

let matched = switch Optional(5) {
case .some(let n): n
case .none: 0
}

let captured = take { [c = matched] in c }

let loops = if Bool.random() {
  take {
    var total = 0
    outer: for i in 0 ..< 3 {
      for j in 0 ..< 3 {
        if j == 1 { continue outer }
        total += i * j
      }
    }
    return total
  }
} else { 0 }

let attributed = take {
  @GA func helper() -> Int { 1 }
  return 2
}

let lazily = take { lazy var v: Int = 7
                    return v }
let wrapped = take { @Wrap var w: Int = 3
                     return w }

let deferred = take {
  defer { print("done") }
  return 8
}

let nested = take { take { 2 } }

let first = take { 1 }, second = take { 2 }

@main
struct App {
  static func main() {
    print(ifExpr, switchExpr, interpolated, interpolatedInBranch, matched)
    print(captured, loops, attributed, deferred)
    print(lazily, wrapped, nested, first, second)
  }
}
