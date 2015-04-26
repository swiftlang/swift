SWIFT_CLASS("_TtC8comments4A000")
@interface A000
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end



/// Aaa.  A010.  Bbb.
SWIFT_CLASS("_TtC8comments21A010_AttachToEntities")
@interface A010_AttachToEntities

/// Aaa.  init().
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
- (NSInteger)objectAtIndexedSubscript:(NSInteger)i;
- (void)setObject:(NSInteger)newValue atIndexedSubscript:(NSInteger)i;

/// Aaa.  v1.
@property (nonatomic) NSInteger v1;

/// Aaa.  v2.
+ (NSInteger)v2;
@end



/// Aaa.  A013.
SWIFT_PROTOCOL("_TtP8comments21A013_AttachToEntities_")
@protocol A013_AttachToEntities
@end


SWIFT_CLASS("_TtC8comments10ATXHeaders")
@interface ATXHeaders

/// <h1>LEVEL ONE</h1>
/// <h2>LEVEL TWO</h2>
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13AutomaticLink")
@interface AutomaticLink

/// And now for a URL.
///
/// <a href="http://developer.apple.com/swift/">http://developer.apple.com/swift/</a>
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments10BlockQuote")
@interface BlockQuote

/// Aaa.
///
/// <blockquote>Bbb.</blockquote>
/// <blockquote>Ccc.</blockquote>
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments5Brief")
@interface Brief

/// Aaa.
- (void)f0;

/// Aaa.
///
/// Bbb.
- (void)f1;

/// Aaa.
///
/// <blockquote>Bbb.</blockquote>
- (void)f2;

/// Aaa.
///
/// Bbb.
- (void)f3;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments9CodeBlock")
@interface CodeBlock

/// This is how you use this code.
///
/// <code> f0() // WOW!

/// </code>
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments8Emphasis")
@interface Emphasis

/// Aaa *bbb* ccc. Aaa *bbb* ccc.
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13EmptyComments")
@interface EmptyComments

- (void)f0;

/// Aaa.
- (void)f1;

- (void)f2;

- (void)f3;

/// Aaa.
- (void)f4;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments15HorizontalRules")
@interface HorizontalRules

/// Briefly.
///
/// <hr/>
/// The end.
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments16ImplicitNameLink")
@interface ImplicitNameLink

/// Apple
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments10InlineCode")
@interface InlineCode

/// Aaa ``bbb`` ccc.
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments10InlineLink")
@interface InlineLink

/// Aaa bbb ccc.
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments11OrderedList")
@interface OrderedList

/// <ol><li>Aaa.</li><li>Bbb.
/// Ccc.</li></ol>
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end



/// \param x A number
SWIFT_CLASS("_TtC8comments15ParamAndReturns")
@interface ParamAndReturns

/// Aaa.  f0.
///
/// \param first Bbb.
///
/// \param second Ccc.  Ddd.
/// Eee.
- (void)f0:(NSInteger)first second:(double)second;

/// Aaa.  f1.
///
/// \param first Bbb.
///
/// \returns  Ccc.
/// Ddd.
- (void)f1:(NSInteger)first;

/// Aaa.  f2.
///
/// \param first 
///
/// \param second Aaa.
///
/// \param third 
/// Bbb.
- (void)f2:(NSInteger)first second:(double)second third:(float)third;

/// Aaa.  f3.
///
/// \param first Bbb.
///
/// \param second Ccc.
///
/// \param third Ddd.
- (void)f3:(NSInteger)first second:(double)second third:(float)third;

/// Aaa.  f4.
///
/// \returns  Eee.
/// Fff.
- (void)f4;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments16ParameterOutline")
@interface ParameterOutline

/// \param x A number
///
/// \param y A number
///
/// \param z A number
- (void)f0:(NSInteger)x y:(NSInteger)y z:(NSInteger)z;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments22ParameterOutlineMiddle")
@interface ParameterOutlineMiddle

/// <ul><li>This line should remain.</li><li>This line should also remain.</li></ul>
/// \param x A number
///
/// \param y A number
///
/// \param z A number
- (void)f0:(NSInteger)x y:(NSInteger)y z:(NSInteger)z;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13ReferenceLink")
@interface ReferenceLink
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments7Returns")
@interface Returns

/// \returns  A number
- (NSInteger)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments18SeparateParameters")
@interface SeparateParameters

/// \param x A number
- (void)f0:(NSInteger)x y:(NSInteger)y;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13SetextHeaders")
@interface SetextHeaders

/// <h1>LEVEL ONE</h1>
/// <h2>LEVEL TWO</h2>
/// <h3>LEVEL THREE</h3>
/// <h4>LEVEL FOUR</h4>
/// <h5>LEVEL FIVE</h5>
/// <h5>LEVEL SIX</h5>
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments14StrongEmphasis")
@interface StrongEmphasis

/// Aaa **bbb** ccc. Aaa **bbb** ccc.
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13UnorderedList")
@interface UnorderedList

/// <ul><li>Aaa.</li><li>Bbb.
/// Ccc.</li></ul>
/// <ul><li>Ddd.</li><li>Eee.<ul><li>Fff.</li></ul></li></ul>
- (void)f0;
- (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end

#pragma clang diagnostic pop
