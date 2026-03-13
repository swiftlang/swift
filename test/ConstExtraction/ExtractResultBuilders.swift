// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/inputs)
// RUN: echo "[FooProvider]" > %t/inputs/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractResultBuilders.swiftconstvalues -const-gather-protocols-file %t/inputs/protocols.json -primary-file %s
// RUN: cat %t/ExtractResultBuilders.swiftconstvalues 2>&1 | %FileCheck %s

public struct Foo {
    let name: String
    let baz: String = ""

    public init(name: String) {
        self.name = name
    }

    public init(name: String, @SimpleStringBuilder baz: () -> String) {
        self.name = name
        self.baz = baz()
    }
}

@resultBuilder
struct SimpleStringBuilder {
    static func buildBlock(_ parts: String...) -> String {
        parts.joined(separator: "\n")
    }
}

@resultBuilder
public enum FooBuilder {
    public typealias Component = [Foo]
    public typealias Expression = Foo

    public static func buildExpression(_ element: Expression) -> Component {
        return [element]
    }

    public static func buildOptional(_ component: Component?) -> Component {
        guard let component = component else {
            return []
        }
        return component
    }

    public static func buildEither(first component: Component) -> Component {
        return component
    }

    public static func buildEither(second component: Component) -> Component {
        return component
    }

    public static func buildArray(_ components: [Component]) -> Component {
        return Array(components.joined())
    }

    public static func buildBlock(_ components: Component...) -> Component {
        return components.flatMap { $0 }
    }

    public static func buildLimitedAvailability(_ component: Component) -> Component {
        return component
    }
}

public protocol FooProvider {
    @FooBuilder
    static var foos: [Foo] { get }
}

public struct MyFooProvider: FooProvider {
    @FooBuilder
    public static var foos: [Foo] {
        Foo(name: "MyFooProvider.foos.1")
        Foo(name: "MyFooProvider.foos.2")
    }

    @FooBuilder
    public static var bars: [Foo] {
        Foo(name: "MyFooProvider.bars.1")
        Foo(name: "MyFooProvider.bars.2")
    }
}

let someNumber = Int.random(in: 0...10)

public struct MyFooProviderInferred: FooProvider {
    public static var foos: [Foo] {
        Foo(name: "MyFooProviderInferred.foos.1", baz: {
            "Nested.Builder.1"
            "Nested.Builder.2"
        })
        Foo(name: "MyFooProviderInferred.foos.2")

        if someNumber < 3 {
            Foo(name: "MyFooProviderInferred.foos.if.LessThan3")
        } else if someNumber < 7 {
            Foo(name: "MyFooProviderInferred.foos.elseif.Between3And6")
        } else {
            Foo(name: "MyFooProviderInferred.foos.else.Between7And10")
        }

        for i in 1...3 {
            Foo(name: "MyFooProviderInferred.foos.Array.\(i)")
        }

        if Bool.random() {
            Foo(name: "MyFooProviderInferred.foos.Optional")
        }

        if #available(iOS 18.0, macOS 15.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
            Foo(name: "MyFooProviderInferred.foos.limitedAvailability.1")
            Foo(name: "MyFooProviderInferred.foos.limitedAvailability.2")
        } else {
            Foo(name: "MyFooProviderInferred.foos.limitedAvailability.else")
        }
    }
}

public struct MyFooProviderInferredWithArrayInitialization: FooProvider {
    public static var foos: [Foo] = [
        Foo(name: "MyFooProviderInferredWithArrayInitialization.foos.1", baz: {
            "Nested.Builder.1"
            "Nested.Builder.2"
        }),
        Foo(name: "MyFooProviderInferredWithArrayInitialization.foos.2"),
    ]
}

public struct MyFooProviderInferredWithArrayReturn: FooProvider {
    public static var foos: [Foo] {
        return [
            Foo(name: "MyFooProviderInferredWithArrayReturn.foos.1"),
            Foo(name: "MyFooProviderInferredWithArrayInitialization.foos.2"),
        ]
    }
}

// CHECK: [
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractResultBuilders.MyFooProvider",
// CHECK-NEXT:     "mangledTypeName": "21ExtractResultBuilders13MyFooProviderV",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:     "line": 71,
// CHECK-NEXT:     "conformances": [
// CHECK-NEXT:       "ExtractResultBuilders.FooProvider"
// CHECK-NEXT:     ],
// CHECK-NEXT:    "allConformances": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "protocolName": "ExtractResultBuilders.FooProvider"
// CHECK-NEXT:        "conformanceDefiningModule": "ExtractResultBuilders"
// CHECK-NEXT:      }
// CHECK-NEXT:    ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "foos",
// CHECK-NEXT:         "type": "Swift.Array<ExtractResultBuilders.Foo>",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "true",
// CHECK-NEXT:         "isComputed": "true",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:         "line": 73,
// CHECK-NEXT:         "valueKind": "Builder",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "ExtractResultBuilders.FooBuilder",
// CHECK-NEXT:           "members": [
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildExpression",
// CHECK-NEXT:               "element": {
// CHECK-NEXT:                 "valueKind": "InitCall",
// CHECK-NEXT:                 "value": {
// CHECK-NEXT:                   "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                   "arguments": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "name",
// CHECK-NEXT:                       "type": "Swift.String",
// CHECK-NEXT:                       "valueKind": "RawLiteral",
// CHECK-NEXT:                       "value": "MyFooProvider.foos.1"
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               }
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildExpression",
// CHECK-NEXT:               "element": {
// CHECK-NEXT:                 "valueKind": "InitCall",
// CHECK-NEXT:                 "value": {
// CHECK-NEXT:                   "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                   "arguments": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "name",
// CHECK-NEXT:                       "type": "Swift.String",
// CHECK-NEXT:                       "valueKind": "RawLiteral",
// CHECK-NEXT:                       "value": "MyFooProvider.foos.2"
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               }
// CHECK-NEXT:             }
// CHECK-NEXT:           ]
// CHECK-NEXT:         }
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "bars",
// CHECK-NEXT:         "type": "Swift.Array<ExtractResultBuilders.Foo>",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "true",
// CHECK-NEXT:         "isComputed": "true",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:         "line": 79,
// CHECK-NEXT:         "valueKind": "Builder",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "ExtractResultBuilders.FooBuilder",
// CHECK-NEXT:           "members": [
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildExpression",
// CHECK-NEXT:               "element": {
// CHECK-NEXT:                 "valueKind": "InitCall",
// CHECK-NEXT:                 "value": {
// CHECK-NEXT:                   "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                   "arguments": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "name",
// CHECK-NEXT:                       "type": "Swift.String",
// CHECK-NEXT:                       "valueKind": "RawLiteral",
// CHECK-NEXT:                       "value": "MyFooProvider.bars.1"
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               }
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildExpression",
// CHECK-NEXT:               "element": {
// CHECK-NEXT:                 "valueKind": "InitCall",
// CHECK-NEXT:                 "value": {
// CHECK-NEXT:                   "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                   "arguments": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "name",
// CHECK-NEXT:                       "type": "Swift.String",
// CHECK-NEXT:                       "valueKind": "RawLiteral",
// CHECK-NEXT:                       "value": "MyFooProvider.bars.2"
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               }
// CHECK-NEXT:             }
// CHECK-NEXT:           ]
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractResultBuilders.MyFooProviderInferred",
// CHECK-NEXT:     "mangledTypeName": "21ExtractResultBuilders21MyFooProviderInferredV",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:     "line": 87,
// CHECK-NEXT:     "conformances": [
// CHECK-NEXT:       "ExtractResultBuilders.FooProvider"
// CHECK-NEXT:     ],
// CHECK-NEXT:    "allConformances": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "protocolName": "ExtractResultBuilders.FooProvider"
// CHECK-NEXT:        "conformanceDefiningModule": "ExtractResultBuilders"
// CHECK-NEXT:      }
// CHECK-NEXT:    ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "foos",
// CHECK-NEXT:         "type": "Swift.Array<ExtractResultBuilders.Foo>",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "true",
// CHECK-NEXT:         "isComputed": "true",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:         "line": 88,
// CHECK-NEXT:         "valueKind": "Builder",
// CHECK-NEXT:         "value": {
// CHECK-NEXT:           "type": "ExtractResultBuilders.FooBuilder",
// CHECK-NEXT:           "members": [
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildExpression",
// CHECK-NEXT:               "element": {
// CHECK-NEXT:                 "valueKind": "InitCall",
// CHECK-NEXT:                 "value": {
// CHECK-NEXT:                   "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                   "arguments": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "name",
// CHECK-NEXT:                       "type": "Swift.String",
// CHECK-NEXT:                       "valueKind": "RawLiteral",
// CHECK-NEXT:                       "value": "MyFooProviderInferred.foos.1"
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "baz",
// CHECK-NEXT:                       "type": "() -> Swift.String",
// CHECK-NEXT:                       "valueKind": "Builder",
// CHECK-NEXT:                       "value": {
// CHECK-NEXT:                         "type": "",
// CHECK-NEXT:                         "members": [
// CHECK-NEXT:                           {
// CHECK-NEXT:                             "kind": "buildExpression",
// CHECK-NEXT:                             "element": {
// CHECK-NEXT:                               "valueKind": "RawLiteral",
// CHECK-NEXT:                               "value": "Nested.Builder.1"
// CHECK-NEXT:                             }
// CHECK-NEXT:                           },
// CHECK-NEXT:                           {
// CHECK-NEXT:                             "kind": "buildExpression",
// CHECK-NEXT:                             "element": {
// CHECK-NEXT:                               "valueKind": "RawLiteral",
// CHECK-NEXT:                               "value": "Nested.Builder.2"
// CHECK-NEXT:                             }
// CHECK-NEXT:                           }
// CHECK-NEXT:                         ]
// CHECK-NEXT:                       }
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               }
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildExpression",
// CHECK-NEXT:               "element": {
// CHECK-NEXT:                 "valueKind": "InitCall",
// CHECK-NEXT:                 "value": {
// CHECK-NEXT:                   "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                   "arguments": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "label": "name",
// CHECK-NEXT:                       "type": "Swift.String",
// CHECK-NEXT:                       "valueKind": "RawLiteral",
// CHECK-NEXT:                       "value": "MyFooProviderInferred.foos.2"
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               }
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildEither",
// CHECK-NEXT:               "ifElements": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {
// CHECK-NEXT:                     "valueKind": "InitCall",
// CHECK-NEXT:                     "value": {
// CHECK-NEXT:                       "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                       "arguments": [
// CHECK-NEXT:                         {
// CHECK-NEXT:                           "label": "name",
// CHECK-NEXT:                           "type": "Swift.String",
// CHECK-NEXT:                           "valueKind": "RawLiteral",
// CHECK-NEXT:                           "value": "MyFooProviderInferred.foos.if.LessThan3"
// CHECK-NEXT:                         }
// CHECK-NEXT:                       ]
// CHECK-NEXT:                     }
// CHECK-NEXT:                   }
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {}
// CHECK-NEXT:                 }
// CHECK-NEXT:               ],
// CHECK-NEXT:               "elseElements": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "ifElements": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "element": {
// CHECK-NEXT:                         "valueKind": "InitCall",
// CHECK-NEXT:                         "value": {
// CHECK-NEXT:                           "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                           "arguments": [
// CHECK-NEXT:                             {
// CHECK-NEXT:                               "label": "name",
// CHECK-NEXT:                               "type": "Swift.String",
// CHECK-NEXT:                               "valueKind": "RawLiteral",
// CHECK-NEXT:                               "value": "MyFooProviderInferred.foos.elseif.Between3And6"
// CHECK-NEXT:                             }
// CHECK-NEXT:                           ]
// CHECK-NEXT:                         }
// CHECK-NEXT:                       }
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "element": {}
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ],
// CHECK-NEXT:                   "elseElements": [
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "element": {
// CHECK-NEXT:                         "valueKind": "InitCall",
// CHECK-NEXT:                         "value": {
// CHECK-NEXT:                           "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                           "arguments": [
// CHECK-NEXT:                             {
// CHECK-NEXT:                               "label": "name",
// CHECK-NEXT:                               "type": "Swift.String",
// CHECK-NEXT:                               "valueKind": "RawLiteral",
// CHECK-NEXT:                               "value": "MyFooProviderInferred.foos.else.Between7And10"
// CHECK-NEXT:                             }
// CHECK-NEXT:                           ]
// CHECK-NEXT:                         }
// CHECK-NEXT:                       }
// CHECK-NEXT:                     },
// CHECK-NEXT:                     {
// CHECK-NEXT:                       "element": {}
// CHECK-NEXT:                     }
// CHECK-NEXT:                   ]
// CHECK-NEXT:                 }
// CHECK-NEXT:               ]
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildArray",
// CHECK-NEXT:               "elements": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {
// CHECK-NEXT:                     "valueKind": "InitCall",
// CHECK-NEXT:                     "value": {
// CHECK-NEXT:                       "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                       "arguments": [
// CHECK-NEXT:                         {
// CHECK-NEXT:                           "label": "name",
// CHECK-NEXT:                           "type": "Swift.String",
// CHECK-NEXT:                           "valueKind": "InterpolatedStringLiteral",
// CHECK-NEXT:                           "value": {
// CHECK-NEXT:                             "segments": [
// CHECK-NEXT:                               {
// CHECK-NEXT:                                 "valueKind": "RawLiteral",
// CHECK-NEXT:                                 "value": "MyFooProviderInferred.foos.Array."
// CHECK-NEXT:                               },
// CHECK-NEXT:                               {
// CHECK-NEXT:                                 "valueKind": "Runtime"
// CHECK-NEXT:                               },
// CHECK-NEXT:                               {
// CHECK-NEXT:                                 "valueKind": "RawLiteral",
// CHECK-NEXT:                                 "value": ""
// CHECK-NEXT:                               }
// CHECK-NEXT:                             ]
// CHECK-NEXT:                           }
// CHECK-NEXT:                         }
// CHECK-NEXT:                       ]
// CHECK-NEXT:                     }
// CHECK-NEXT:                   }
// CHECK-NEXT:                 }
// CHECK-NEXT:               ]
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildOptional",
// CHECK-NEXT:               "ifElements": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {
// CHECK-NEXT:                     "valueKind": "InitCall",
// CHECK-NEXT:                     "value": {
// CHECK-NEXT:                       "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                       "arguments": [
// CHECK-NEXT:                         {
// CHECK-NEXT:                           "label": "name",
// CHECK-NEXT:                           "type": "Swift.String",
// CHECK-NEXT:                           "valueKind": "RawLiteral",
// CHECK-NEXT:                           "value": "MyFooProviderInferred.foos.Optional"
// CHECK-NEXT:                         }
// CHECK-NEXT:                       ]
// CHECK-NEXT:                     }
// CHECK-NEXT:                   }
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {}
// CHECK-NEXT:                 }
// CHECK-NEXT:               ],
// CHECK-NEXT:               "elseElements": []
// CHECK-NEXT:             },
// CHECK-NEXT:             {
// CHECK-NEXT:               "kind": "buildLimitedAvailability",
// CHECK-NEXT:               "availabilityAttributes": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "platform": "iOS",
// CHECK-NEXT:                   "minVersion": "18.0"
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "platform": "macOS",
// CHECK-NEXT:                   "minVersion": "15.0"
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "platform": "watchOS",
// CHECK-NEXT:                   "minVersion": "11.0"
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "platform": "tvOS",
// CHECK-NEXT:                   "minVersion": "18.0"
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "platform": "visionOS",
// CHECK-NEXT:                   "minVersion": "2.0"
// CHECK-NEXT:                 }
// CHECK-NEXT:               ],
// CHECK-NEXT:               "ifElements": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {
// CHECK-NEXT:                     "valueKind": "InitCall",
// CHECK-NEXT:                     "value": {
// CHECK-NEXT:                       "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                       "arguments": [
// CHECK-NEXT:                         {
// CHECK-NEXT:                           "label": "name",
// CHECK-NEXT:                           "type": "Swift.String",
// CHECK-NEXT:                           "valueKind": "RawLiteral",
// CHECK-NEXT:                           "value": "MyFooProviderInferred.foos.limitedAvailability.1"
// CHECK-NEXT:                         }
// CHECK-NEXT:                       ]
// CHECK-NEXT:                     }
// CHECK-NEXT:                   }
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {
// CHECK-NEXT:                     "valueKind": "InitCall",
// CHECK-NEXT:                     "value": {
// CHECK-NEXT:                       "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                       "arguments": [
// CHECK-NEXT:                         {
// CHECK-NEXT:                           "label": "name",
// CHECK-NEXT:                           "type": "Swift.String",
// CHECK-NEXT:                           "valueKind": "RawLiteral",
// CHECK-NEXT:                           "value": "MyFooProviderInferred.foos.limitedAvailability.2"
// CHECK-NEXT:                         }
// CHECK-NEXT:                       ]
// CHECK-NEXT:                     }
// CHECK-NEXT:                   }
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {}
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {}
// CHECK-NEXT:                 }
// CHECK-NEXT:               ],
// CHECK-NEXT:               "elseElements": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {
// CHECK-NEXT:                     "valueKind": "InitCall",
// CHECK-NEXT:                     "value": {
// CHECK-NEXT:                       "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:                       "arguments": [
// CHECK-NEXT:                         {
// CHECK-NEXT:                           "label": "name",
// CHECK-NEXT:                           "type": "Swift.String",
// CHECK-NEXT:                           "valueKind": "RawLiteral",
// CHECK-NEXT:                           "value": "MyFooProviderInferred.foos.limitedAvailability.else"
// CHECK-NEXT:                         }
// CHECK-NEXT:                       ]
// CHECK-NEXT:                     }
// CHECK-NEXT:                   }
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "element": {}
// CHECK-NEXT:                 }
// CHECK-NEXT:               ]
// CHECK-NEXT:             }
// CHECK-NEXT:           ]
// CHECK-NEXT:         }
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractResultBuilders.MyFooProviderInferredWithArrayInitialization",
// CHECK-NEXT:     "mangledTypeName": "21ExtractResultBuilders44MyFooProviderInferredWithArrayInitializationV",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:     "line": 120,
// CHECK-NEXT:     "conformances": [
// CHECK-NEXT:       "ExtractResultBuilders.FooProvider"
// CHECK-NEXT:     ],
// CHECK-NEXT:     "allConformances": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "protocolName": "ExtractResultBuilders.FooProvider",
// CHECK-NEXT:         "conformanceDefiningModule": "ExtractResultBuilders"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "foos",
// CHECK-NEXT:         "type": "Swift.Array<ExtractResultBuilders.Foo>",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "true",
// CHECK-NEXT:         "isComputed": "false",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:         "line": 121,
// CHECK-NEXT:         "valueKind": "Array",
// CHECK-NEXT:         "value": [
// CHECK-NEXT:           {
// CHECK-NEXT:             "valueKind": "InitCall",
// CHECK-NEXT:             "value": {
// CHECK-NEXT:               "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:               "arguments": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "label": "name",
// CHECK-NEXT:                   "type": "Swift.String",
// CHECK-NEXT:                   "valueKind": "RawLiteral",
// CHECK-NEXT:                   "value": "MyFooProviderInferredWithArrayInitialization.foos.1"
// CHECK-NEXT:                 },
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "label": "baz",
// CHECK-NEXT:                   "type": "() -> Swift.String",
// CHECK-NEXT:                   "valueKind": "Builder",
// CHECK-NEXT:                   "value": {
// CHECK-NEXT:                     "type": "",
// CHECK-NEXT:                     "members": [
// CHECK-NEXT:                       {
// CHECK-NEXT:                         "kind": "buildExpression",
// CHECK-NEXT:                         "element": {
// CHECK-NEXT:                           "valueKind": "RawLiteral",
// CHECK-NEXT:                           "value": "Nested.Builder.1"
// CHECK-NEXT:                         }
// CHECK-NEXT:                       },
// CHECK-NEXT:                       {
// CHECK-NEXT:                         "kind": "buildExpression",
// CHECK-NEXT:                         "element": {
// CHECK-NEXT:                           "valueKind": "RawLiteral",
// CHECK-NEXT:                           "value": "Nested.Builder.2"
// CHECK-NEXT:                         }
// CHECK-NEXT:                       }
// CHECK-NEXT:                     ]
// CHECK-NEXT:                   }
// CHECK-NEXT:                 }
// CHECK-NEXT:               ]
// CHECK-NEXT:             }
// CHECK-NEXT:           },
// CHECK-NEXT:           {
// CHECK-NEXT:             "valueKind": "InitCall",
// CHECK-NEXT:             "value": {
// CHECK-NEXT:               "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:               "arguments": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "label": "name",
// CHECK-NEXT:                   "type": "Swift.String",
// CHECK-NEXT:                   "valueKind": "RawLiteral",
// CHECK-NEXT:                   "value": "MyFooProviderInferredWithArrayInitialization.foos.2"
// CHECK-NEXT:                 }
// CHECK-NEXT:               ]
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:         ]
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "ExtractResultBuilders.MyFooProviderInferredWithArrayReturn",
// CHECK-NEXT:     "mangledTypeName": "21ExtractResultBuilders36MyFooProviderInferredWithArrayReturnV",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:     "line": 130,
// CHECK-NEXT:     "conformances": [
// CHECK-NEXT:       "ExtractResultBuilders.FooProvider"
// CHECK-NEXT:     ],
// CHECK-NEXT:     "allConformances": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "protocolName": "ExtractResultBuilders.FooProvider",
// CHECK-NEXT:         "conformanceDefiningModule": "ExtractResultBuilders"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "properties": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "label": "foos",
// CHECK-NEXT:         "type": "Swift.Array<ExtractResultBuilders.Foo>",
// CHECK-NEXT:         "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:         "isStatic": "true",
// CHECK-NEXT:         "isComputed": "true",
// CHECK-NEXT:         "file": "{{.*}}test{{/|\\\\}}ConstExtraction{{/|\\\\}}ExtractResultBuilders.swift",
// CHECK-NEXT:         "line": 131,
// CHECK-NEXT:         "valueKind": "Array",
// CHECK-NEXT:         "value": [
// CHECK-NEXT:           {
// CHECK-NEXT:             "valueKind": "InitCall",
// CHECK-NEXT:             "value": {
// CHECK-NEXT:               "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:               "arguments": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "label": "name",
// CHECK-NEXT:                   "type": "Swift.String",
// CHECK-NEXT:                   "valueKind": "RawLiteral",
// CHECK-NEXT:                   "value": "MyFooProviderInferredWithArrayReturn.foos.1"
// CHECK-NEXT:                 }
// CHECK-NEXT:               ]
// CHECK-NEXT:             }
// CHECK-NEXT:           },
// CHECK-NEXT:           {
// CHECK-NEXT:             "valueKind": "InitCall",
// CHECK-NEXT:             "value": {
// CHECK-NEXT:               "type": "ExtractResultBuilders.Foo",
// CHECK-NEXT:               "arguments": [
// CHECK-NEXT:                 {
// CHECK-NEXT:                   "label": "name",
// CHECK-NEXT:                   "type": "Swift.String",
// CHECK-NEXT:                   "valueKind": "RawLiteral",
// CHECK-NEXT:                   "value": "MyFooProviderInferredWithArrayInitialization.foos.2"
// CHECK-NEXT:                 }
// CHECK-NEXT:               ]
// CHECK-NEXT:             }
// CHECK-NEXT:           }
// CHECK-NEXT:         ]
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   }
// CHECK-NEXT: ]
