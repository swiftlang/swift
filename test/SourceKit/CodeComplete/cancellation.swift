// Try and schedule the cancel the cancellation as fast as possible to cancel during the first pass
// RUN: not %sourcekitd-test -req=complete -pos 15:35 %s -id=complete -async -- %s == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_NO_CACHE %s

// Wait 1 second for the first pass to complet and try to cancel during the second pass. This relies on the fact that the expression in line 12 is slow to type check (rdar://80582770)
// RUN: not %sourcekitd-test -req=complete -pos 15:35 %s -id=complete -async -- %s == -shell -- sleep 1 == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_NO_CACHE %s

// Built an AST inside `fast(a:)` then complete the slow operation and try to cancel it.
// RUN: not %sourcekitd-test -req=complete -pos 23:7 %s -- %s == -req=complete -pos 15:35 %s -id=complete -async -- %s == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_CACHED %s

// Same as above but sleep 1 second before cancelling to make sure we are actually cancelling during the second pass.
// RUN: not %sourcekitd-test -req=complete -pos 23:7 %s -- %s == -req=complete -pos 15:35 %s -id=complete -async -- %s == -shell -- sleep 1 == -cancel=complete 2>&1 | %FileCheck --check-prefix=CANCEL_CACHED %s

class Foo {
  func slow(x: Invalid1, y: Invalid2) {
    x / y / x / y / x / y / x / y.
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
