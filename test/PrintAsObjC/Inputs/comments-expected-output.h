SWIFT_CLASS("_TtC8comments4A000", "comments")
@interface A000
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


/// Aaa.  A010.  Bbb.
SWIFT_CLASS("_TtC8comments21A010_AttachToEntities", "comments")
@interface A010_AttachToEntities
/// Aaa.  init().
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
- (NSInteger)objectAtIndexedSubscript:(NSInteger)i SWIFT_WARN_UNUSED_RESULT;
- (void)setObject:(NSInteger)newValue atIndexedSubscript:(NSInteger)i;
/// Aaa.  v1.
@property (nonatomic) NSInteger v1;
/// Aaa.  v2.
SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly) NSInteger v2;)
+ (NSInteger)v2 SWIFT_WARN_UNUSED_RESULT;
@end


/// Aaa.  A013.
SWIFT_PROTOCOL("_TtP8comments21A013_AttachToEntities_", "comments")
@protocol A013_AttachToEntities
@end


SWIFT_CLASS("_TtC8comments10ATXHeaders", "comments")
@interface ATXHeaders
/// <h1>LEVEL ONE</h1>
/// <h2>LEVEL TWO</h2>
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13AutomaticLink", "comments")
@interface AutomaticLink
/// And now for a URL.
/// <a href="http://developer.apple.com/swift/">http://developer.apple.com/swift/</a>
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments10BlockQuote", "comments")
@interface BlockQuote
/// Aaa.
/// <blockquote>
/// Bbb.
///
/// </blockquote>
/// <blockquote>
/// Ccc.
///
/// </blockquote>
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments5Brief", "comments")
@interface Brief
/// Aaa.
- (void)f0;
/// Aaa.
/// Bbb.
- (void)f1;
/// Aaa.
/// <blockquote>
/// Bbb.
///
/// </blockquote>
- (void)f2;
/// Aaa.
/// Bbb.
- (void)f3;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments15ClosingComments", "comments")
@interface ClosingComments
/// Some comment. */
- (void)closingComment;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments16ClosureContainer", "comments")
@interface ClosureContainer
/// Partially applies a binary operator.
/// \param a The left-hand side to partially apply.
///
/// \param combine A binary operator.
///
/// \a combine parameters:
/// <ul>
/// <li>
/// lhs: The left-hand side of the operator
/// </li>
/// <li>
/// rhs: The right-hand side of the operator
/// </li>
/// </ul>
///
///
/// \a combine returns: A result.
///
/// \a combine error: Nothing.
///
- (void)closureParameterExplodedExplodedWithA:(NSInteger)a combine:(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(NSInteger, NSInteger))combine;
/// Partially applies a binary operator.
/// \param a The left-hand side to partially apply.
///
/// \param combine A binary operator.
///
/// \a combine parameters:
/// <ul>
/// <li>
/// lhs: The left-hand side of the operator
/// </li>
/// <li>
/// rhs: The right-hand side of the operator
/// </li>
/// </ul>
///
///
/// \a combine returns: A result.
///
/// \a combine error: Nothing.
///
- (void)closureParameterOutlineExplodedWithA:(NSInteger)a combine:(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(NSInteger, NSInteger))combine;
/// Partially applies a binary operator.
/// \param a The left-hand side to partially apply.
///
/// \param combine A binary operator.
///
/// \a combine parameters:
/// <ul>
/// <li>
/// lhs: The left-hand side of the operator
/// </li>
/// <li>
/// rhs: The right-hand side of the operator
/// </li>
/// </ul>
///
///
/// \a combine returns: A result.
///
/// \a combine error: Nothing.
///
- (void)closureParameterOutlineOutlineWithA:(NSInteger)a combine:(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(NSInteger, NSInteger))combine;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments9CodeBlock", "comments")
@interface CodeBlock
/// This is how you use this code.
/// \code
/// f0() // WOW!
/// f0() // WOW!
/// f0() // WOW!
///
/// \endcode
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments8Emphasis", "comments")
@interface Emphasis
/// Aaa <em>bbb</em> ccc.
/// Aaa <em>bbb</em> ccc.
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13EmptyComments", "comments")
@interface EmptyComments
///
- (void)f0;
/// Aaa.
- (void)f1;
///
- (void)f2;
///
- (void)f3;
/// Aaa.
- (void)f4;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments19HasThrowingFunction", "comments")
@interface HasThrowingFunction
/// Might throw something.
/// \param x A number
///
///
/// throws:
/// An error if <code>x == 0</code>
- (void)f1:(NSInteger)x;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments15HorizontalRules", "comments")
@interface HorizontalRules
/// Briefly.
/// <hr/>
/// The end.
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments16ImplicitNameLink", "comments")
@interface ImplicitNameLink
/// <a href="https://www.apple.com/">Apple</a>
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments20IndentedBlockComment", "comments")
@interface IndentedBlockComment
/// Brief.
/// First paragraph line.
/// Second paragraph line.
/// Now for a code sample:
/// \code
/// var x = 1
/// // var y = 2
/// var z = 3
///
/// \endcode
- (void)f1;
/// Hugely indented brief.
/// First paragraph line.
/// Second paragraph line.
/// Now for a code sample:
/// \code
/// var x = 1
/// // var y = 2
/// var z = 3
///
/// \endcode
- (void)f2;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments10InlineCode", "comments")
@interface InlineCode
/// Aaa <code>bbb</code> ccc.
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments10InlineLink", "comments")
@interface InlineLink
/// Aaa <a href="/path/to/something">bbb</a> ccc.
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments14MultiLineBrief", "comments")
@interface MultiLineBrief
/// Brief first line.
/// Brief after softbreak.
/// Some paragraph text.
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments11OrderedList", "comments")
@interface OrderedList
/// <ol>
///   <li>
///     Aaa.
///   </li>
///   <li>
///     Bbb.
///     Ccc.
///   </li>
/// </ol>
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


/// \param x A number
///
SWIFT_CLASS("_TtC8comments15ParamAndReturns", "comments")
@interface ParamAndReturns
/// Aaa.  f0.
/// \param first Bbb.
///
/// \param second Ccc.  Ddd.
/// Eee.
///
- (void)f0:(NSInteger)first second:(double)second;
/// Aaa.  f1.
/// \param first Bbb.
///
///
/// returns:
/// Ccc.
/// Ddd.
- (void)f1:(NSInteger)first;
/// Aaa.  f2.
/// \param first 
///
/// \param second Aaa.
///
/// \param third 
/// Bbb.
///
- (void)f2:(NSInteger)first second:(double)second third:(float)third;
/// Aaa.  f3.
/// \param first Bbb.
///
/// \param second Ccc.
///
/// \param third Ddd.
///
- (void)f3:(NSInteger)first second:(double)second third:(float)third;
/// Aaa.  f4.
///
/// returns:
/// Eee.
/// Fff.
- (void)f4;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments16ParameterOutline", "comments")
@interface ParameterOutline
/// \param x A number
///
/// \param y A number
///
/// \param z A number
///
- (void)f0:(NSInteger)x y:(NSInteger)y z:(NSInteger)z;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments22ParameterOutlineMiddle", "comments")
@interface ParameterOutlineMiddle
/// <ul>
///   <li>
///     This line should remain.
///   </li>
///   <li>
///     This line should also remain.
///   </li>
/// </ul>
/// \param x A number
///
/// \param y A number
///
/// \param z A number
///
- (void)f0:(NSInteger)x y:(NSInteger)y z:(NSInteger)z;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13ReferenceLink", "comments")
@interface ReferenceLink
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments7Returns", "comments")
@interface Returns
///
/// returns:
/// A number
- (NSInteger)f0 SWIFT_WARN_UNUSED_RESULT;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments18SeparateParameters", "comments")
@interface SeparateParameters
/// \param x A number
///
- (void)f0:(NSInteger)x y:(NSInteger)y;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13SetextHeaders", "comments")
@interface SetextHeaders
/// <h1>LEVEL ONE</h1>
/// <h2>LEVEL TWO</h2>
/// <h3>LEVEL THREE</h3>
/// <h4>LEVEL FOUR</h4>
/// <h5>LEVEL FIVE</h5>
/// <h5>LEVEL SIX</h5>
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments14StrongEmphasis", "comments")
@interface StrongEmphasis
/// Aaa <em>bbb</em> ccc.
/// Aaa <em>bbb</em> ccc.
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13UnorderedList", "comments")
@interface UnorderedList
/// <ul>
///   <li>
///     Aaa.
///   </li>
///   <li>
///     Bbb.
///     Ccc.
///   </li>
/// </ul>
/// <ul>
///   <li>
///     Ddd.
///   </li>
///   <li>
///     Eee.
///     <ul>
///       <li>
///         Fff.
///       </li>
///     </ul>
///   </li>
/// </ul>
- (void)f0;
- (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end

#pragma clang diagnostic pop
