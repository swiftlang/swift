// RUN: %batch-code-completion -enable-experimental-feature InlineArrayTypeSugar

// REQUIRES: swift_feature_InlineArrayTypeSugar

struct FooBar {}

[3 of #^COMPLETE_TOPLEVEL?check=COMPLETE^#
let _: [3 of #^COMPLETE_TYPE?check=COMPLETE^#
// COMPLETE: Decl[Struct]/CurrModule: FooBar[#FooBar#]; name=FooBar
