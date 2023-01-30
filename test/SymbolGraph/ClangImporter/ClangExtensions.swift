// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/EmitWhileBuilding/EmitWhileBuilding.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/EmitWhileBuilding.framework/Modules/EmitWhileBuilding.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name EmitWhileBuilding -disable-objc-attr-requires-foundation-module %s %S/Inputs/EmitWhileBuilding/Extra.swift -emit-symbol-graph -emit-symbol-graph-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json --check-prefix TYPE
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json --check-prefix EXTENSION
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/EmitWhileBuilding@EmitWhileBuilding.symbols.json

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name EmitWhileBuilding -F %t -output-dir %t -pretty-print -v -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json --check-prefix TYPE
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json --check-prefix EXTENSION
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/EmitWhileBuilding@EmitWhileBuilding.symbols.json

// REQUIRES: objc_interop

// ensure that the symbol `Foo.Bar` does appear in the base module's symbol graph
// and that there is no "swift.extension" symbol there

// TYPE: "s:So3FooV17EmitWhileBuildingE3BarO",
// EXTENSION-NOT: swift.extension

public extension Foo {
    enum Bar { }
}
