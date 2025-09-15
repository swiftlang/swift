// RUN: %target-swift-frontend -enable-experimental-feature RawLayout -parse -verify %s

// REQUIRES: swift_feature_RawLayout

@_rawLayout(size: 4, alignment: 4)
struct Lock: ~Copyable {}

@_rawLayout(like: Int)
struct Lock2: ~Copyable {}

@_rawLayout(like: Optional<Int>)
struct Lock3: ~Copyable {}

@_rawLayout(like: T)
struct MyUnmanaged<T>: ~Copyable {}

@_rawLayout(likeArrayOf: T, count: 8)
struct SmallVectorBuf<T>: ~Copyable {}

@_rawLayout // expected-error{{expected '('}}
struct NoLayoutSpecified: ~Copyable {}

@_rawLayout() // expected-error{{expected 'size', 'like', or 'likeArrayOf' argument to '@_rawLayout'}}
struct NoParamsSpecified: ~Copyable {}

@_rawLayout(size: 4) // expected-error{{expected alignment argument after size argument in '@_rawLayout'}}
struct SizeWithoutAlignment: ~Copyable {}

@_rawLayout(likeArrayOf: Optional<Int>) // expected-error{{expected count argument after likeArrayOf argument in '@_rawLayout'}}
struct ArrayWithoutSize: ~Copyable {}

