// BEGIN main.swift
enum E { case foo, bar }
func foo(x: (E) -> Void) {}
func test() {
  foo(x: <#T##(E) -> Void#>)
}

// BEGIN expand.json.in
{
    key.request: source.request.editor.expand_placeholder,
    key.offset: 23,
    key.length: 18,
    key.name: "FILENAME"
}

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: sed "s#FILENAME#%t/main.swift#" %t/expand.json.in > %t/expand.json

// RUN: %sourcekitd-test \
// RUN:   -req=open %t/main.swift -- %t/main.swift == \
// RUN:   -req=edit -offset=0 -length=53 -replace="" -req-opts=enablesyntaxmap=0,enablesubstructure=0,enablediagnostics=0 %t/main.swift -- %t/main.swift == \
// RUN:   -json-request-path %t/expand.json \
// RUN: | %FileCheck %s

// CHECK: {
// CHECK:   key.offset: 19,
// CHECK:   key.length: 23,
// CHECK:   key.sourcetext: " { <#E#> in\n<#code#>\n}"
// CHECK: }

