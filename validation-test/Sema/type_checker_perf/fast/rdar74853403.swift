// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

func makeString(_ strings: [String]) -> String { "" }
func makeString(_ string: String) -> String { "" }

func test(message: inout String, d: [String: String]?) {
  message += d.map {
    $0.reduce("") {
      $0 + makeString($1.key) + "" + makeString($1.value) + "" + makeString($1.key) + ""
    }
  } ?? ""
}

