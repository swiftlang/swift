// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -emit-library -module-name Types %S/Inputs/fixits-derived-conformances-multifile.swift -o %t/%target-library-name(Types)
// RUN: %swift -typecheck -target %target-triple -I %t -diagnostics-editor-mode -verify %s

import Types

extension GenericEnum: Equatable { }
// expected-error@-1 {{extension outside of file declaring generic enum 'GenericEnum' prevents automatic synthesis of '==' for protocol 'Equatable'}}
// expected-note@-2 {{do you want to add protocol stubs?}}{{35-35=\n    public static func == (lhs: GenericEnum, rhs: GenericEnum) -> Bool {\n        <#code#>\n    \}\n}}

extension Struct: Equatable { }
// expected-error@-1 {{extension outside of file declaring struct 'Struct' prevents automatic synthesis of '==' for protocol 'Equatable'}}
// expected-note@-2 {{do you want to add protocol stubs?}}{{30-30=\n    public static func == (lhs: Struct, rhs: Struct) -> Bool {\n        <#code#>\n    \}\n}}
extension GenericStruct: Equatable { }
// expected-error@-1 {{extension outside of file declaring generic struct 'GenericStruct' prevents automatic synthesis of '==' for protocol 'Equatable'}}
// expected-note@-2 {{do you want to add protocol stubs?}}{{37-37=\n    public static func == (lhs: GenericStruct, rhs: GenericStruct) -> Bool {\n        <#code#>\n    \}\n}}

extension Enum: CaseIterable { }
// expected-error@-1 {{extension outside of file declaring enum 'Enum' prevents automatic synthesis of 'allCases' for protocol 'CaseIterable'}}
// expected-note@-2 {{do you want to add protocol stubs?}}{{31-31=\n    public static var allCases: [Enum]\n}}

