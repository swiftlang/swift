// RUN: %sourcekitd-test -req=cursor -pos=9:6 %s -- %s | %FileCheck -check-prefix=CHECK_HOMOGENEOUS_PARAM %s
// RUN: %sourcekitd-test -req=cursor -pos=12:6 %s -- %s | %FileCheck -check-prefix=CHECK_HOMOGENEOUS_RETURN %s
// RUN: %sourcekitd-test -req=cursor -pos=15:5 %s -- %s | %FileCheck -check-prefix=CHECK_HOMOGENEOUS_GLOBAL %s
// RUN: %sourcekitd-test -req=cursor -pos=18:6 %s -- %s | %FileCheck -check-prefix=CHECK_SMALL %s
// RUN: %sourcekitd-test -req=cursor -pos=21:6 %s -- %s | %FileCheck -check-prefix=CHECK_LABELED %s
// RUN: %sourcekitd-test -req=cursor -pos=24:6 %s -- %s | %FileCheck -check-prefix=CHECK_HETEROGENEOUS %s

// 5-element homogeneous unlabeled tuple as a parameter type compacts.
func homogeneousParam(_ x: (Int, Int, Int, Int, Int)) {}

// 5-element homogeneous unlabeled tuple as a return type compacts.
func homogeneousReturn() -> (Int, Int, Int, Int, Int) { fatalError() }

// 5-element homogeneous unlabeled tuple as a global var type compacts.
let homogeneousGlobal: (Int, Int, Int, Int, Int) = (0, 0, 0, 0, 0)

// 4-element homogeneous unlabeled tuple does not compact (below threshold).
func smallTuple(_ x: (Int, Int, Int, Int)) {}

// 5-element labeled homogeneous tuple does not compact (labels disqualify).
func labeledTuple(_ x: (a: Int, b: Int, c: Int, d: Int, e: Int)) {}

// 5-element heterogeneous tuple does not compact (not homogeneous).
func heterogeneousTuple(_ x: (Int, Int, Int, Int, String)) {}

// CHECK_HOMOGENEOUS_PARAM: <Declaration>func homogeneousParam(_ x: (<Type usr="s:Si">Int</Type> /* ... repeated 5 times ... */))</Declaration>
// CHECK_HOMOGENEOUS_PARAM: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>homogeneousParam</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>x</decl.var.parameter.name>: <decl.var.parameter.type><tuple>(<ref.struct usr="s:Si">Int</ref.struct> /* ... repeated 5 times ... */)</tuple></decl.var.parameter.type></decl.var.parameter>)</decl.function.free>

// CHECK_HOMOGENEOUS_RETURN: <Declaration>func homogeneousReturn() -&gt; (<Type usr="s:Si">Int</Type> /* ... repeated 5 times ... */)</Declaration>
// CHECK_HOMOGENEOUS_RETURN: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>homogeneousReturn</decl.name>() -&gt; <decl.function.returntype><tuple>(<ref.struct usr="s:Si">Int</ref.struct> /* ... repeated 5 times ... */)</tuple></decl.function.returntype></decl.function.free>

// CHECK_HOMOGENEOUS_GLOBAL: <Declaration>let homogeneousGlobal: (<Type usr="s:Si">Int</Type> /* ... repeated 5 times ... */)</Declaration>
// CHECK_HOMOGENEOUS_GLOBAL: <decl.var.global><syntaxtype.keyword>let</syntaxtype.keyword> <decl.name>homogeneousGlobal</decl.name>: <decl.var.type><tuple>(<ref.struct usr="s:Si">Int</ref.struct> /* ... repeated 5 times ... */)</tuple></decl.var.type></decl.var.global>

// CHECK_SMALL: <Declaration>func smallTuple(_ x: (<Type usr="s:Si">Int</Type>, <Type usr="s:Si">Int</Type>, <Type usr="s:Si">Int</Type>, <Type usr="s:Si">Int</Type>))</Declaration>

// CHECK_LABELED: <Declaration>func labeledTuple(_ x: (a: <Type usr="s:Si">Int</Type>, b: <Type usr="s:Si">Int</Type>, c: <Type usr="s:Si">Int</Type>, d: <Type usr="s:Si">Int</Type>, e: <Type usr="s:Si">Int</Type>))</Declaration>

// CHECK_HETEROGENEOUS: <Declaration>func heterogeneousTuple(_ x: (<Type usr="s:Si">Int</Type>, <Type usr="s:Si">Int</Type>, <Type usr="s:Si">Int</Type>, <Type usr="s:Si">Int</Type>, <Type usr="s:SS">String</Type>))</Declaration>