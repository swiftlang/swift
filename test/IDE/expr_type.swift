// RUN: %target-swift-ide-test -print-expr-type -source-filename %S/Inputs/ExprType.swift -swift-version 5 | %FileCheck %s

// CHECK: func foo() -> Int { return <expr type:"Int">1</expr> }
// CHECK: func bar(f: Float) -> Float { return <expr type:"Float"><expr type:"(Float) -> Float">bar</expr><expr type:"(f: Float)">(f: <expr type:"Float">1</expr>)</expr></expr> }
// CHECK: func fooP(_ p: P) { <expr type:"()"><expr type:"(P) -> ()">fooP</expr><expr type:"(P)">(<expr type:"P">p</expr>)</expr></expr> }
// CHECK: <expr type:"()"><expr type:"Int">_</expr> = <expr type:"Int"><expr type:"[C]">a</expr>.count</expr></expr>
// CHECK: <expr type:"()"><expr type:"String">_</expr> = <expr type:"String"><expr type:"Int"><expr type:"(Int) -> Int"><expr type:"Int"><expr type:"String"><expr type:"[C]">a</expr>.description</expr>.count</expr>.<expr type:"(Int) -> (Int) -> Int">advanced</expr></expr><expr type:"(by: Int)">(by: <expr type:"Int">1</expr>)</expr></expr>.description</expr></expr>
// CHECK: <expr type:"()"><expr type:"Int?">_</expr> = <expr type:"Int?"><expr type:"Int"><expr type:"(Int) -> Int"><expr type:"Int"><expr type:"S"><expr type:"S?"><expr type:"[Int : S]">a</expr><expr type:"(Int)">[<expr type:"Int">2</expr>]</expr></expr>?</expr>.val</expr>.<expr type:"(Int) -> (Int) -> Int">advanced</expr></expr><expr type:"(by: Int)">(by: <expr type:"Int">1</expr>)</expr></expr>.byteSwapped</expr></expr>
