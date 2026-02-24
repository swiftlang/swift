// RUN: %target-typecheck-verify-swift

// Tests for the 'and' and 'or' contextual keyword operators, which are
// word-form aliases for '&&' and '||'.  When followed by a leading-dot
// expression (.method(args)), the receiver of the preceding call is used as
// the implicit base.

// ---------------------------------------------------------------------------
// Basic 'and' / 'or' as aliases for '&&' / '||'
// ---------------------------------------------------------------------------

func boolAnd(_ a: Bool, _ b: Bool) -> Bool { a && b }
func boolOr(_ a: Bool, _ b: Bool) -> Bool { a || b }

let t = true
let f = false

let _ = t and f          // equivalent to t && f
let _ = t or f           // equivalent to t || f
let _ = t and t or f     // equivalent to t && t || f
let _ = (t and f) or t   // equivalent to (t && f) || t

// ---------------------------------------------------------------------------
// Implicit-receiver syntax: `expr and .method(args)`
// ---------------------------------------------------------------------------

let hw = "Hello, World!"

// Simple case: check the same string for two substrings.
let _ = hw.contains("Hello") or .contains("World")   // hw.contains("Hello") || hw.contains("World")
let _ = hw.contains("Hello") and .contains("World")  // hw.contains("Hello") && hw.contains("World")

// Chained method with intermediate result.
let words = ["swift", "language"]
let _ = words.contains("swift") or .contains("language")

// Accessing .isEmpty as a property (no call suffix on LHS).
struct Wrapper {
    var flag: Bool
    func check() -> Bool { flag }
}
let w = Wrapper(flag: true)
// 'and' / 'or' without implicit-receiver (plain boolean operands).
let _ = w.check() and w.check()
let _ = w.check() or w.check()

// ---------------------------------------------------------------------------
// 'and' / 'or' used as variable names (contextual keywords, not reserved)
// ---------------------------------------------------------------------------

let and = true   // 'and' can still be used as an identifier
let or = false   // 'or' can still be used as an identifier
let _ = and || or
