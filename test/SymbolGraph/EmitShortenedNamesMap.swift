// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name EmitShortenedNames -emit-module -emit-module-path %t/EmitShortenedNames.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -symbol-graph-shorten-output-names
// RUN: %FileCheck %s --input-file %t/9d0ee7243b44e35c186695a49461007b.symbols.json --check-prefix BASE
// RUN: %FileCheck %s --input-file %t/27a36c452b5f5e2b488b7755cecf40c7.symbols.json --check-prefix EXT
// RUN: %FileCheck %s --input-file %t/symbol_modules.json --check-prefix MAP

// now try with the swift-symbolgraph-extract tool directly

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name EmitShortenedNames -emit-module -emit-module-path %t/EmitShortenedNames.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name EmitShortenedNames -I %t -output-dir %t -symbol-graph-shorten-output-names
// RUN: %FileCheck %s --input-file %t/9d0ee7243b44e35c186695a49461007b.symbols.json --check-prefix BASE
// RUN: %FileCheck %s --input-file %t/27a36c452b5f5e2b488b7755cecf40c7.symbols.json --check-prefix EXT
// RUN: %FileCheck %s --input-file %t/symbol_modules.json --check-prefix MAP

// now try with the swiftc driver

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver %s -module-name EmitShortenedNames -emit-module -emit-module-path %t/EmitShortenedNames.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -symbol-graph-shorten-output-names
// RUN: %FileCheck %s --input-file %t/9d0ee7243b44e35c186695a49461007b.symbols.json --check-prefix BASE
// RUN: %FileCheck %s --input-file %t/27a36c452b5f5e2b488b7755cecf40c7.symbols.json --check-prefix EXT
// RUN: %FileCheck %s --input-file %t/symbol_modules.json --check-prefix MAP

public func foo() {}

extension Sequence {
    public func bar() {}
}

// BASE: "precise":"s:18EmitShortenedNames3fooyyF"
// EXT: "precise":"s:ST18EmitShortenedNamesE3baryyF"
// MAP: "EmitShortenedNames":"9d0ee7243b44e35c186695a49461007b.symbols.json",
// MAP: "EmitShortenedNames@Swift":"27a36c452b5f5e2b488b7755cecf40c7.symbols.json"
