public class Base {}

public protocol Proto {}

public func foo() -> some Base & Proto {
  class Derived: Base, Proto {}
  return Derived()
}

// RUN: %sourcekitd-test -req=cursor -pos=5:13 %s -- %s | %FileCheck %s
// CHECK: <Declaration>public func foo() -&gt; some <Type usr=[[Base_USR:.*]]>Base</Type> &amp; <Type usr=[[Proto_USR:.*]]>Proto</Type></Declaration>
// CHECK: <decl.function.free><syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>() -&gt; <decl.function.returntype><syntaxtype.keyword>some</syntaxtype.keyword> <ref.class usr=[[Base_USR]]>Base</ref.class> &amp; <ref.protocol usr=[[Proto_USR]]>Proto</ref.protocol></decl.function.returntype></decl.function.free>
