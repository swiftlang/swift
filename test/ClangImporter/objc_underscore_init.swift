// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated -import-objc-header %S/Inputs/objc_underscore_init.h %s

// REQUIRES: objc_interop

import Foundation

func test() {
    // Bare `-_initWithOther:` is imported as `init(_other:)`; the leading
    // underscore is preserved on the argument label so the SPI marker
    // stays visible at the use site.
    _ = UnderscoreInitTest(_other: 1)

    // Explicit `swift_name(init(customLabel:))` is honored. The label
    // differs from what the implicit naming path would produce
    // (`init(_otherSwiftName:)`), so this fails if the annotation is dropped.
    _ = UnderscoreInitTest(customLabel: 2)

    // `objc_method_family(init)` is honored.
    _ = UnderscoreInitTest(_otherFamily: 3)

    // Multi-piece selector: first label is `_foo`, second stays plain.
    _ = UnderscoreInitTest(_foo: 1, bar: 2)

    // Zero-arg `-_init` imports as `init()` (no spurious label).
    _ = UnderscoreInitTest()

    // Unlike `_other:` above, `NSArray *` matches the trailing `Array`
    // word, routing this through `omitNeedlessWordsInFunctionName` after
    // the underscore is prepended. Guards against the label collapsing
    // to `init(_:)`.
    _ = UnderscoreInitTest(_array: [])

    // Class methods are never imported as initializers, regardless of the
    // family attribute. They are imported as class methods instead — the
    // standard ObjC method-name splitting applies.
    _ = UnderscoreInitTest._init(fromClassMethod: 1)
}


