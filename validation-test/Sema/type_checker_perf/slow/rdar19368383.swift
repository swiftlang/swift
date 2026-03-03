// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

// Invalid expression: Missing force of optional result of dictionary lookup.
func rdar19368383(d: [String : String]) -> [String] {
  var r = [String]()
  r += [ // expected-error {{reasonable time}}
    "1" + d["2"] + "3",
    "1" + d["2"] + "3",
    "1" + d["2"] + "3",
    "1" + d["2"] + "3",
    "1" + d["2"] + "3",
    "1" + d["2"] + "3",
  ]
  return r
}
