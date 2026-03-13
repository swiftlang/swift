// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Function -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Function -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Function.symbols.json --match-full-lines --strict-whitespace --check-prefix=FOO
// RUN: %FileCheck %s --input-file %t/Function.symbols.json --match-full-lines --strict-whitespace --check-prefix=BAR

public func foo<S>(f: @escaping () -> (), ext int: Int = 2, s: S) where S: Sequence {}

// FOO-LABEL:        "precise": "s:8Function3foo1f3ext1syyyc_SixtSTRzlF",
// FOO:{{^      }}"declarationFragments": [
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "keyword",
// FOO-NEXT:          "spelling": "func"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": " "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "identifier",
// FOO-NEXT:          "spelling": "foo"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": "<"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "genericParameter",
// FOO-NEXT:          "spelling": "S"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": ">("
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "externalParam",
// FOO-NEXT:          "spelling": "f"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": ": "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "attribute",
// FOO-NEXT:          "spelling": "@escaping "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": "() -> (), "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "externalParam",
// FOO-NEXT:          "spelling": "ext"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": " "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "internalParam",
// FOO-NEXT:          "spelling": "int"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": ": "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "typeIdentifier",
// FOO-NEXT:          "spelling": "Int",
// FOO-NEXT:          "preciseIdentifier": "s:Si"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": " = 2, "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "externalParam",
// FOO-NEXT:          "spelling": "s"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": ": "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "typeIdentifier",
// FOO-NEXT:          "spelling": "S"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": ") "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "keyword",
// FOO-NEXT:          "spelling": "where"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": " "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "typeIdentifier",
// FOO-NEXT:          "spelling": "S"
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "text",
// FOO-NEXT:          "spelling": " : "
// FOO-NEXT:        },
// FOO-NEXT:        {
// FOO-NEXT:          "kind": "typeIdentifier",
// FOO-NEXT:          "spelling": "Sequence",
// FOO-NEXT:          "preciseIdentifier": "s:ST"
// FOO-NEXT:        }
// FOO-NEXT:      ],

public func bar<T>(_ apply: () -> T, onChange: @autoclosure () -> @Sendable () -> Void) {}

// BAR-LABEL:        "precise": "s:8Function3bar_8onChangeyxyXE_yyYbcyXKtlF",
// BAR:      "declarationFragments": [
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "keyword",
// BAR-NEXT:          "spelling": "func"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": " "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "identifier",
// BAR-NEXT:          "spelling": "bar"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": "<"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "genericParameter",
// BAR-NEXT:          "spelling": "T"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": ">("
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "externalParam",
// BAR-NEXT:          "spelling": "_"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": " "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "internalParam",
// BAR-NEXT:          "spelling": "apply"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": ": () -> "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "typeIdentifier",
// BAR-NEXT:          "spelling": "T"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": ", "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "externalParam",
// BAR-NEXT:          "spelling": "onChange"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": ": "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "attribute",
// BAR-NEXT:          "spelling": "@autoclosure "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": "() -> "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "attribute",
// BAR-NEXT:          "spelling": "@Sendable"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": " () -> "
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "typeIdentifier",
// BAR-NEXT:          "spelling": "Void",
// BAR-NEXT:          "preciseIdentifier": "s:s4Voida"
// BAR-NEXT:        },
// BAR-NEXT:        {
// BAR-NEXT:          "kind": "text",
// BAR-NEXT:          "spelling": ")"
// BAR-NEXT:        }
// BAR-NEXT:      ],
