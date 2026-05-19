@class NSArray;

@interface UnderscoreInitTest
- (instancetype)init;

// Bare SPI-prefixed init: Clang classifies this as `OMF_init` family;
// importer should recognize the `_init`-prefixed selector and import as
// an initializer with a `_`-prefixed first argument label.
- (instancetype)_initWithOther:(int)x;

// Explicit `swift_name(init(...))` should be honored on a `_init`-prefixed
// selector; the user's custom argument label is preserved.
- (instancetype)_initWithOtherSwiftName:(int)x
    __attribute__((swift_name("init(_otherSwiftName:)")));

// `objc_method_family(init)` on a `_init`-prefixed selector should be
// honored too.
- (instancetype)_initWithOtherFamily:(int)x
    __attribute__((objc_method_family(init)));

// Multi-piece selector: only the first label gets the underscore prefix.
- (instancetype)_initWithFoo:(int)foo bar:(int)bar;

// Zero-arg `-_init` (just the prefix, no suffix). Imports as `init()`,
// shadowing the inherited no-arg init.
- (instancetype)_init __attribute__((objc_method_family(init)));

// `-_initWithArray:` exercises the `omitNeedlessWords` path: without the
// underscore-preserve fix the omission could collapse the label to `init(_:)`.
// With the fix, the leading `_` is retained: `init(_array:)`.
- (instancetype)_initWithArray:(NSArray *)arr;

// Class methods are never imported as initializers, regardless of the
// underscore prefix or family attribute.
+ (instancetype)_initFromClassMethod:(int)x
    __attribute__((objc_method_family(init)));
@end
