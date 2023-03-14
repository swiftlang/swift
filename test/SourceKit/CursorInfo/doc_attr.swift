@_documentation(visibility: public)
enum E
{
}

// The @_documentation attribute caused sourcekit-lsp to crash
// cf. https://github.com/apple/swift/issues/64309

// RUN: %sourcekitd-test -req=cursor -pos=2:6 %s -- %s | %FileCheck %s

// CHECK: <Declaration>@_documentation(visibility: public) enum E</Declaration>
// CHECK: <decl.enum><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@_documentation</syntaxtype.attribute.name>(visibility: public)</syntaxtype.attribute.builtin> <syntaxtype.keyword>enum</syntaxtype.keyword> <decl.name>E</decl.name></decl.enum>
