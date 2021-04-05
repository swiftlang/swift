// RUN: %target-typecheck-verify-swift -swift-version 5 -solver-expression-time-threshold=1

func method(_ arg: String, body: () -> [String]) {}

func test(str: String, properties: [String]) {
  // expected-error@+1 {{the compiler is unable to type-check this expression in reasonable time}}
  method(str + "" + str + "") {
    properties.map { param in
      "" + param + "" + param + "" + param + ""
    } + [""]
  }
}
