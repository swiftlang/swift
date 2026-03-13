@_spi(Hello)
public struct World {}

// RUN: %sourcekitd-test -req=cursor -pos=2:15 %s -- %s | %FileCheck -check-prefix=CHECK %s
// CHECK: source.lang.swift.decl.struct (2:15-2:20)
// CHECK: <Declaration>@_spi(Hello) public struct World</Declaration>
// CHECK: <decl.struct><syntaxtype.attribute.name>@_spi</syntaxtype.attribute.name>(Hello) <syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>World</decl.name></decl.struct>
