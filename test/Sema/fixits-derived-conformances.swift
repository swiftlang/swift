// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -emit-library -module-name Types %S/Inputs/fixits-derived-conformances-multifile.swift -o %t/%target-library-name(Types)
// RUN: %target-typecheck-verify-swift %s -I %t

import Types

extension GenericEnum: @retroactive Equatable { }
// expected-error@-1 {{extension outside of file declaring generic enum 'GenericEnum' prevents automatic synthesis of '==' for protocol 'Equatable'}}
// expected-note@-2 {{add stubs for conformance}}{{48-48=\n    public static func == (lhs: GenericEnum, rhs: GenericEnum) -> Bool {\n        <#code#>\n    \}\n}}

extension Struct: @retroactive Equatable { }
// expected-error@-1 {{extension outside of file declaring struct 'Struct' prevents automatic synthesis of '==' for protocol 'Equatable'}}
// expected-note@-2 {{add stubs for conformance}}{{43-43=\n    public static func == (lhs: Struct, rhs: Struct) -> Bool {\n        <#code#>\n    \}\n}}
extension GenericStruct: @retroactive Equatable { }
// expected-error@-1 {{extension outside of file declaring generic struct 'GenericStruct' prevents automatic synthesis of '==' for protocol 'Equatable'}}
// expected-note@-2 {{add stubs for conformance}}{{50-50=\n    public static func == (lhs: GenericStruct, rhs: GenericStruct) -> Bool {\n        <#code#>\n    \}\n}}

extension Enum: @retroactive CaseIterable { }
// expected-error@-1 {{extension outside of file declaring enum 'Enum' prevents automatic synthesis of 'allCases' for protocol 'CaseIterable'}}
// expected-note@-2 {{add stubs for conformance}}{{44-44=\n    public static let allCases: [Enum]\n}}

