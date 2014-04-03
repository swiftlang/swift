@interface A000
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end



/// Aaa.  A10.  Bbb.
SWIFT_CLASS
@interface A010

- (void)f0;

/// Aaa.
- (void)f1;

- (void)f2;

- (void)f3;

/// <ul><li><p>Aaa.</p></li></ul>
- (void)f4;

/// Aaa.  f5.
///
/// \param first Bbb.
///
/// \param second Ccc.  Ddd.
/// Eee.
- (void)f5:(NSInteger)first second:(double)second;

/// Aaa.  f6.
///
/// \param first Bbb.
///
/// \returns Ccc.
/// Ddd.
- (void)f6:(NSInteger)first;

/// Aaa.  f7.
///
/// \returns Ccc.
/// Ddd.
///
/// \returns Eee.
/// Fff.
- (void)f7;

/// Aaa.  init().
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
- (NSInteger)objectAtIndexedSubscript:(NSInteger)i;
- (void)setObject:(NSInteger)newValue atIndexedSubscript:(NSInteger)i;

/// Aaa.  v1.
@property (nonatomic) NSInteger v1;
@end


SWIFT_CLASS
@interface A020_BulletList

/// <ul><li><p>Aaa.</p></li><li><p>Bbb.
/// Ccc.</p></li></ul>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS
@interface A030_EnumeratedList

/// <ol><li><p>Aaa.</p></li><li><p>Bbb.
/// Ccc.</p></li></ol>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS
@interface A040_DefinitionList

/// <dl><dt>Aaa</dt><dd><p>Bbb.</p></dd><dt>Ccc</dt><dd><p>Ddd.</p></dd><dt>Eee : Fff</dt><dd><p>Ggg.</p></dd><dt>``Hhh``</dt><dd><p>Jjj.</p></dd></dl>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS
@interface A050_FieldList

/// <dl><dt>unknown</dt><dd><p>Aaa.
/// Bbb.</p></dd></dl>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS
@interface A060_OptionList

/// -a   Aaa.
/// -b   Bbb.
///
/// <blockquote><p>Ccc.</p></blockquote>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


SWIFT_CLASS
@interface A070_BlockQuote

/// Aaa.
///
/// <blockquote><p>Bbb.</p><p>Ccc.</p></blockquote>
- (void)f0;
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end


