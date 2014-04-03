@interface A000
- (instancetype)init OBJC_DESIGNATED_INITIALIZER;
@end



/// Aaa.  A1.  Bbb.
SWIFT_CLASS
@interface A1

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


