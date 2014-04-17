SWIFT_CLASS("_TtC8comments4A000")
@interface A000
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end



/// Aaa.  A010.  Bbb.
SWIFT_CLASS("_TtC8comments21A010_AttachToEntities")
@interface A010_AttachToEntities

/// Aaa.  init().
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
- (NSInteger)objectAtIndexedSubscript:(NSInteger)i;
- (void)setObject:(NSInteger)newValue atIndexedSubscript:(NSInteger)i;

/// Aaa.  v1.
@property (nonatomic) NSInteger v1;
@end


SWIFT_PROTOCOL("_TtP8comments21A013_AttachToEntities_")
@protocol A013_AttachToEntities
@end


SWIFT_CLASS("_TtC8comments18A100_EmptyComments")
@interface A100_EmptyComments

- (void)f0;

/// Aaa.
- (void)f1;

- (void)f2;

- (void)f3;

/// <ul><li><p>Aaa.</p></li></ul>
- (void)f4;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments13A110_Escaping")
@interface A110_Escaping

/// & < > " '
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments10A120_Brief")
@interface A120_Brief

/// Aaa.
- (void)f0;

/// Aaa.
///
/// Bbb.
- (void)f1;

/// Aaa.
///
/// <blockquote><p>Bbb.</p></blockquote>
- (void)f2;

/// Aaa.
///
/// Bbb.
- (void)f3;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments20A200_ParamAndReturns")
@interface A200_ParamAndReturns

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
/// \returns Ccc.
/// Ddd.
- (void)f1:(NSInteger)first;

/// Aaa.  f2.
///
/// \returns Ccc.
/// Ddd.
///
/// \returns Eee.
/// Fff.
- (void)f2;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments15A210_BulletList")
@interface A210_BulletList

/// <ul><li><p>Aaa.</p></li><li><p>Bbb.
/// Ccc.</p></li></ul>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments19A220_EnumeratedList")
@interface A220_EnumeratedList

/// <ol><li><p>Aaa.</p></li><li><p>Bbb.
/// Ccc.</p></li></ol>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments19A230_DefinitionList")
@interface A230_DefinitionList

/// <dl><dt>Aaa</dt><dd><p>Bbb.</p></dd><dt>Ccc</dt><dd><p>Ddd.</p></dd><dt>Eee : Fff</dt><dd><p>Ggg.</p></dd><dt>``Hhh``</dt><dd><p>Jjj.</p></dd></dl>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments14A240_FieldList")
@interface A240_FieldList

/// <dl><dt>unknown</dt><dd><p>Aaa.
/// Bbb.</p></dd></dl>
- (void)f0;

/// <ul><li><p>Aaa.</p><dl><dt>param</dt><dd><p>Aaa.
/// :returns: Bbb.
/// :unknown: Ccc.</p></dd></dl></li></ul>
- (void)f1;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments15A250_OptionList")
@interface A250_OptionList

/// -a   Aaa.
/// -b   Bbb.
///
/// <blockquote><p>Ccc.</p></blockquote>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS("_TtC8comments15A260_BlockQuote")
@interface A260_BlockQuote

/// Aaa.
///
/// <blockquote><p>Bbb.</p><p>Ccc.</p></blockquote>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


