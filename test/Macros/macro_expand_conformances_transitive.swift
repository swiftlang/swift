// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Verify that transitive conformances from extension macros are visible
// from other files in the same module. An extension macro generating
// conformance to DerivedProto (which inherits BaseProto) should make
// BaseProto's members accessible from any file.

// Primary file mode:
// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser %t/a.swift -primary-file %t/b.swift

// Whole-module mode:
// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser %t/a.swift %t/b.swift

//--- a.swift

protocol BaseProto {
  static var tableName: String { get }
}

extension BaseProto {
  static func deleteOne(key: String) -> Bool {
    return true
  }
}

protocol DerivedProto: BaseProto {}

@attached(extension, conformances: DerivedProto)
macro TransitiveConformance() = #externalMacro(module: "MacroDefinition", type: "TransitiveConformanceViaExtensionMacro")

@TransitiveConformance
struct Item {
  static let tableName = "items"
}

//--- b.swift

// The conformance to DerivedProto (and transitively BaseProto) is generated
// by the extension macro applied to Item in a.swift. Calling a method
// available through BaseProto should work here.
func test() {
  _ = Item.deleteOne(key: "item-1")
}
