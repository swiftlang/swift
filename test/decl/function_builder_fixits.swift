// RUN: %target-typecheck-verify-swift -disable-availability-checking
// UNSUPPORTED: windows
// Line-feeds in Fix-Its fail to check on Windows.

@_functionBuilder
struct Maker {} // expected-error {{function builder must provide at least one static 'buildBlock' method}}{{15-15=\n    static func buildBlock(_ components: <#Component#>...) -> <#Component#> {\n      <#code#>\n    \}}}

@_functionBuilder
struct TupleBuilderWithoutIf { // expected-note 3{{struct 'TupleBuilderWithoutIf' declared here}}
  // expected-note@-1{{add 'buildOptional(_:)' to the function builder 'TupleBuilderWithoutIf' to add support for 'if' statements without an 'else'}}{{31-31=\n    static func buildOptional(_ component: <#Component#>?) -> <#Component#> {\n      <#code#>\n    \}}}
  // expected-note@-2{{add 'buildEither(first:)' and 'buildEither(second:)' to the function builder 'TupleBuilderWithoutIf' to add support for 'if'-'else' and 'switch'}}{{31-31=\n    static func buildEither(first component: <#Component#>) -> <#Component#> {\n      <#code#>\n    \}\n\n    static func buildEither(second component: <#Component#>) -> <#Component#> {\n      <#code#>\n    \}}}
  // expected-note@-3{{add 'buildArray(_:)' to the function builder 'TupleBuilderWithoutIf' to add support for 'for'..'in' loops}}{{31-31=\n    static func buildArray(_ components: [<#Component#>]) -> <#Component#> {\n      <#code#>\n    \}}}
  static func buildBlock() -> () { }
  
  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
  }
  
  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }
}

func tuplifyWithoutIf<T>(_ cond: Bool, @TupleBuilderWithoutIf body: (Bool) -> T) {
  print(body(cond))
}

func testDiags() {
  // Statements unsupported by the particular builder.
  tuplifyWithoutIf(true) {
    if $0 {    // expected-error{{closure containing control flow statement cannot be used with function builder 'TupleBuilderWithoutIf'}}
      "hello"
    }
  }

  tuplifyWithoutIf(true) {
    if $0 {    // expected-error{{closure containing control flow statement cannot be used with function builder 'TupleBuilderWithoutIf'}}
      "hello"
    } else {
    }
  }

  tuplifyWithoutIf(true) { a in
    for x in 0..<100 {    // expected-error{{closure containing control flow statement cannot be used with function builder 'TupleBuilderWithoutIf'}}
      x
    }
  }
}
