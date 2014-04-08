__attribute__((objc_root_class))
@interface ImplicitProperties
- (id)implicitProperty;
- (void)setImplicitProperty:(id)implicitProperty;

- (void)setAnotherImplicitProperty:(int)implicitProperty;
- (int)anotherImplicitProperty;
@end

__attribute__((objc_root_class))
@interface BadImplicitProperties
- (int)nonVoidReturn;
- (int)setNonVoidReturn:(int)val;

- (void)setNonMatchingType:(id)val;
- (int)nonMatchingType;

- (int)wrongGetterArgs:(int)val;
- (void)setWrongGetterArgs:(int)val;

- (void)setWrongSetterArgs:(int)val extra:(int)another;
- (int)wrongSetterArgs;

- (int)wrongSetterArgs2;
- (void)setWrongSetterArgs2;

- (int)getterOnly;

- (void)setSetterOnly:(int)val;
@end
