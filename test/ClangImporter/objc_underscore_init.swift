// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated -import-objc-header %S/Inputs/objc_underscore_init.h %s

// REQUIRES: objc_interop

import Foundation

func test() {
    // Bare `-_initWithOther:` is imported as `init(_other:)`; the leading
    // underscore is preserved on the argument label so the SPI marker
    // stays visible at the use site.
    _ = UnderscoreInitTest(_other: 1)

    // Explicit `swift_name(init(_otherSwiftName:))` is honored.
    _ = UnderscoreInitTest(_otherSwiftName: 2)

    // `objc_method_family(init)` is honored.
    _ = UnderscoreInitTest(_otherFamily: 3)

    // Multi-piece selector: first label is `_foo`, second stays plain.
    _ = UnderscoreInitTest(_foo: 1, bar: 2)

    // Zero-arg `-_init` imports as `init()` (no spurious label).
    _ = UnderscoreInitTest()

    // `omitNeedlessWords` doesn't strip the leading underscore.
    _ = UnderscoreInitTest(_array: [])

    // Class methods are never imported as initializers, regardless of the
    // family attribute. They are imported as class methods instead — the
    // standard ObjC method-name splitting applies.
    _ = UnderscoreInitTest._init(fromClassMethod: 1)
}


