// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Navigator -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Navigator -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Navigator.symbols.json --check-prefix=MYSTRUCT
// RUN: %FileCheck %s --input-file %t/Navigator.symbols.json --check-prefix=FOO
// RUN: %FileCheck %s --input-file %t/Navigator.symbols.json --check-prefix=BAR
public struct MyStruct<S> { public var x: S
  public init(x: S) {
    self.x = x
  }
  public func foo() where S: Sequence {}
  public func bar<T>(x: T) where T: Sequence {}
}

// MYSTRUCT-LABEL: "precise": "s:9Navigator8MyStructV"
// MYSTRUCT:        names
// MYSTRUCT-NEXT:      "title": "MyStruct",
// MYSTRUCT-NEXT:      "navigator": [
// MYSTRUCT-NEXT:          {
// MYSTRUCT-NEXT:              "kind": "identifier"
// MYSTRUCT-NEXT:              "spelling": "MyStruct"
// MYSTRUCT-NEXT:          }
// MYSTRUCT-NEXT:      ]

// FOO-LABEL: "precise": "s:9Navigator8MyStructV3fooyySTRzlF"
// FOO:        names
// FOO-NEXT:      "title": "foo()",
// FOO-NEXT:      "navigator": [
// FOO-NEXT:          {
// FOO-NEXT:              "kind": "keyword"
// FOO-NEXT:              "spelling": "func"
// FOO-NEXT:          }
// FOO-NEXT:          {
// FOO-NEXT:              "kind": "text"
// FOO-NEXT:              "spelling": " "
// FOO-NEXT:          }
// FOO-NEXT:          {
// FOO-NEXT:              "kind": "identifier"
// FOO-NEXT:              "spelling": "foo"
// FOO-NEXT:          }
// FOO-NEXT:          {
// FOO-NEXT:              "kind": "text"
// FOO-NEXT:              "spelling": "()"
// FOO-NEXT:          }
// FOO-NEXT:      ]

// BAR-LABEL: "precise": "s:9Navigator8MyStructV3bar1xyqd___tSTRd__lF"
// BAR:        names
// BAR-NEXT:      "title": "bar(x:)",
// BAR-NEXT:      "navigator": [
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "keyword",
// BAR-NEXT:            "spelling": "func"
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "text",
// BAR-NEXT:            "spelling": " "
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "identifier",
// BAR-NEXT:            "spelling": "bar"
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "text",
// BAR-NEXT:            "spelling": "<"
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "genericParameter",
// BAR-NEXT:            "spelling": "T"
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "text",
// BAR-NEXT:            "spelling": ">("
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "externalParam",
// BAR-NEXT:            "spelling": "x"
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "text",
// BAR-NEXT:            "spelling": ": T"
// BAR-NEXT:          }
// BAR-NEXT:          {
// BAR-NEXT:            "kind": "text",
// BAR-NEXT:            "spelling": ")"
// BAR-NEXT:          }
// BAR-NEXT:      ]
