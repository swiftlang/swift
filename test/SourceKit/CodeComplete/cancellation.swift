// Try and schedule the cancel the cancellation as fast as possible to cancel during the first pass
// RUN: not %sourcekitd-test -req=complete -pos 31:57 %s -id=complete -async -- %s == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_NO_CACHE %s

// Wait 1 second for the first pass to complete and try to cancel during the second pass. This relies on the fact that the expression in line 31 is slow to type check.
// RUN: not %sourcekitd-test -req=complete -pos 31:57 %s -id=complete -async -- %s == -shell -- sleep 1 == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_NO_CACHE %s

// Built an AST inside `fast(a:)` then complete the slow operation and try to cancel it.
// RUN: not %sourcekitd-test -req=complete -pos 39:7 %s -- %s == -req=complete -pos 31:57 %s -id=complete -async -- %s == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_CACHED %s

// Same as above but sleep 1 second before cancelling to make sure we are actually cancelling during the second pass.
// RUN: not %sourcekitd-test -req=complete -pos 39:7 %s -- %s == -req=complete -pos 31:57 %s -id=complete -async -- %s == -shell -- sleep 1 == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_CACHED %s

struct A: ExpressibleByIntegerLiteral { init(integerLiteral value: Int) {} }
struct B: ExpressibleByIntegerLiteral { init(integerLiteral value: Int) {} }
struct C: ExpressibleByIntegerLiteral { init(integerLiteral value: Int) {} }

func + (lhs: A, rhs: B) -> A { fatalError() }
func + (lhs: B, rhs: C) -> A { fatalError() }
func + (lhs: C, rhs: A) -> A { fatalError() }

func + (lhs: B, rhs: A) -> B { fatalError() }
func + (lhs: C, rhs: B) -> B { fatalError() }
func + (lhs: A, rhs: C) -> B { fatalError() }

func + (lhs: C, rhs: B) -> C { fatalError() }
func + (lhs: B, rhs: C) -> C { fatalError() }
func + (lhs: A, rhs: A) -> C { fatalError() }

class Foo {
  func slow() {
    let x: C = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + // don't trim
  }

  struct Foo {
    let fooMember: String
  }

  func fast(a: Foo) {
    a.
  }
}

// CANCEL_NO_CACHE: error response (Request Cancelled)

// CANCEL_CACHED: key.results: [
// CANCEL_CACHED:   fooMember
// CANCEL_CACHED: ]
// CANCEL_CACHED: error response (Request Cancelled)
