// RUN: %sourcekitd-test -req=sema %S/../../decl/protocol/conforms/fixit_stub_opaque_results.swift -- %S/../../decl/protocol/conforms/fixit_stub_opaque_results.swift | %FileCheck %s

// The two alternatives must be separate notes, each with one insertion.
// CHECK: key.description: "type 'MyFactory' does not conform to protocol 'Factory'"
// CHECK: key.id: "missing_witnesses_general",
// CHECK-NEXT: key.description: "add stubs for conformance",
// CHECK-NEXT: key.fixits: [
// CHECK-NEXT: {
// CHECK-NEXT: key.offset: [[FACTORY:[0-9]+]],
// CHECK-NEXT: key.length: 0,
// CHECK-NEXT: key.sourcetext: "\n    typealias Product = <#type#>\n"
// CHECK-NEXT: }
// CHECK-NEXT: ]
// CHECK-NEXT: },
// CHECK: key.id: "missing_witnesses_opaque_results",
// CHECK-NEXT: key.description: "add stubs using opaque result types",
// CHECK-NEXT: key.fixits: [
// CHECK-NEXT: {
// CHECK-NEXT: key.offset: [[FACTORY]],
// CHECK-NEXT: key.length: 0,
// CHECK-NEXT: key.sourcetext: "\n    func make() -> some Widget {\n        <#code#>\n    }\n"
// CHECK-NEXT: }
// CHECK-NEXT: ]
// CHECK-NEXT: },

// CHECK: key.description: "type 'MyFetcher' does not conform to protocol 'Fetcher'"
// CHECK: key.id: "missing_witnesses_general",
// CHECK: key.sourcetext: "\n    typealias Output = <#type#>\n"
// CHECK: key.id: "missing_witnesses_opaque_results",
// CHECK: key.sourcetext: "\n    func fetch(url: String) async throws -> some Promise<any Response> {\n        <#code#>\n    }\n"

// CHECK: key.description: "type 'ExtensionFactory' does not conform to protocol 'PublicFactory'"
// CHECK: key.id: "missing_witnesses_general",
// CHECK: key.sourcetext: "\n    public typealias Product = <#type#>\n"
// CHECK: key.id: "missing_witnesses_opaque_results",
// CHECK: key.sourcetext: "\n    public static func make(_ count: Int) throws -> some Widget {\n        <#code#>\n    }\n"

// The remaining cases keep the original action without an opaque alternative.
// CHECK: key.description: "type 'Shared' does not conform to protocol 'SharedResult'"
// CHECK: key.id: "missing_witnesses_general",
// CHECK-NOT: key.id: "missing_witnesses_opaque_results"
