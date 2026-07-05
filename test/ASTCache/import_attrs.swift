// RUN: rm -rf %t.cache %t
// RUN: mkdir -p %t.cache %t
// RUN: echo '@_spi(Internal) public struct HiddenStruct { public var value: Int; public init(value: Int) { self.value = value } }' > %t/lib.swift
// RUN: %target-swift-frontend -emit-module -module-name testlib -parse-as-library \
// RUN:   -emit-module-path %t/testlib.swiftmodule %t/lib.swift
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -I %t -typecheck %s 2>&1 | %FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -I %t -typecheck %s 2>&1 | %FileCheck %s --check-prefix=WARM

// Negative test: importing testlib WITHOUT @_spi and referencing HiddenStruct
// must fail. This run does NOT use -experimental-ast-cache, so there is no
// cache recovery to suppress the error. This proves SPI enforcement is active
// and that the @_spi attribute matters (not just the module being loaded).
// RUN: echo 'import testlib' > %t/no_spi.swift
// RUN: echo 'func bad() -> Int { return HiddenStruct(value: 1).value }' >> %t/no_spi.swift
// RUN: not %target-swift-frontend -module-name negmod -parse-as-library \
// RUN:   -I %t -typecheck %t/no_spi.swift 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=NEGATIVE

// COLD: AST cache: MISS (no cache file)
// COLD: AST cache: SAVED

// WARM: AST cache: HIT

// NEGATIVE: error: cannot find 'HiddenStruct' in scope

// Test BUG-3 fix: Import attributes preserved during cache deserialization.
// Previously, getImportedModules() dropped @_spi attributes, causing warm
// builds to fail when accessing SPI-gated symbols. With the fix using
// getDependencies(), the @_spi(Internal) attribute is preserved through
// the cache roundtrip, allowing HiddenStruct to be accessed on warm load.
//
// The negative test proves SPI enforcement is active: without @_spi(Internal),
// HiddenStruct is invisible even though testlib is imported.

@_spi(Internal) import testlib

func useSPI() -> Int {
  let h = HiddenStruct(value: 42)
  return h.value
}
