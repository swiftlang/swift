@protocol P
@property (nonatomic, copy, readonly, nullable) id property;
- (void) method;
- (id)objectForKeyedSubscript:(id)subscript;
- (instancetype)initWithFoo:(id)foo;
@end

@protocol Q <P>
@property (nonatomic, copy, readonly, nullable) id property;
- (void) method;
- (id)objectForKeyedSubscript:(id)subscript;
- (instancetype)initWithFoo:(id)foo;
@end

