// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-generated)
// RUN: %target-swift-frontend %S/Inputs/ExportedImportExtensions/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %S/Inputs/ExportedImportExtensions/B.swift -module-name B -emit-module -emit-module-path %t/B.swiftmodule -I %t
// RUN: %target-swift-frontend %s -module-name ExportedImportExtensions -emit-module -emit-module-path \
// RUN:     %t/ExportedImportExtensions.swiftmodule -I %t -emit-symbol-graph -emit-symbol-graph-dir %t/ -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/ExportedImportExtensions.symbols.json
// RUN: %target-swift-symbolgraph-extract -module-name ExportedImportExtensions -I %t -output-dir %t/module-generated/ \
// RUN:     -emit-extension-block-symbols -experimental-allowed-reexported-modules=A,B
// RUN: %FileCheck %s --input-file %t/module-generated/ExportedImportExtensions.symbols.json
// RUN: ls %t | %FileCheck %s --check-prefix FILES
// RUN: ls %t/module-generated/ | %FileCheck %s --check-prefix FILES

@_exported import A
@_exported import B

public protocol P {
    func pRequirement()
}

public extension SymbolFromA {
    func extensionFromMain() { }
}

public extension SymbolFromB {
    func extensionFromMain() { }
}

extension SymbolFromB: P {
    public func pRequirement() { }
}

// CHECK-NOT: "swift.extension"

// the extensions on SymbolFromA ...
// CHECK-DAG: "precise":"s:1A11SymbolFromAV"
// ... made by B
// CHECK-DAG: "precise":"s:1A11SymbolFromAV1BE09extensionB1ByyF"
// CHECK-DAG: {"kind":"memberOf","source":"s:1A11SymbolFromAV1BE09extensionB1ByyF","target":"s:1A11SymbolFromAV","targetFallback":"A.SymbolFromA"}
// ... and by this file
// CHECK-DAG: "precise":"s:1A11SymbolFromAV24ExportedImportExtensionsE09extensionB4MainyyF"
// CHECK-DAG: {"kind":"memberOf","source":"s:1A11SymbolFromAV24ExportedImportExtensionsE09extensionB4MainyyF","target":"s:1A11SymbolFromAV","targetFallback":"A.SymbolFromA"}

// the simple member extension on SymbolFromB (made by this file)
// CHECK-DAG: "precise":"s:1B11SymbolFromBV"
// CHECK-DAG: "precise":"s:1B11SymbolFromBV24ExportedImportExtensionsE09extensionB4MainyyF"
// CHECK-DAG: {"kind":"memberOf","source":"s:1B11SymbolFromBV24ExportedImportExtensionsE09extensionB4MainyyF","target":"s:1B11SymbolFromBV","targetFallback":"B.SymbolFromB"}

// the conformance extension on SymbolFromB (made by this file)
// CHECK-DAG: "precise":"s:1B11SymbolFromBV24ExportedImportExtensionsE12pRequirementyyF"
// CHECK-DAG: {"kind":"memberOf","source":"s:1B11SymbolFromBV24ExportedImportExtensionsE12pRequirementyyF","target":"s:1B11SymbolFromBV","targetFallback":"B.SymbolFromB","sourceOrigin":{"identifier":"s:24ExportedImportExtensions1PP12pRequirementyyF","displayName":"P.pRequirement()"}}
// CHECK-DAG: {"kind":"conformsTo","source":"s:1B11SymbolFromBV","target":"s:24ExportedImportExtensions1PP"}

// FILES-NOT: ExportedImportExtensions@A.symbols.json
// FILES-NOT: ExportedImportExtensions@B.symbols.json
// FILES-NOT: A@B.symbols.json
// FILES-NOT: B@A.symbols.json
// FILES-NOT: A.symbols.json
// FILES-NOT: B.symbols.json
