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


@protocol PropertiesProto
- (id)methodInProto;
@property id propertyInProto;
@end

__attribute__((objc_root_class))
@interface Base <PropertiesProto>
- (id)methodInBase;
@property(readonly) id propertyInBase;

- (id)methodPairInBase;
- (void)setMethodPairInBase:(id)value;

- (id)getterOnlyInBase;

- (void)setSetterOnlyInBase:(id)value;

@property id methodInProto;
- (id)propertyInProto;
- (id)methodInBaseButPropertyInProto;
@property id propertyInBaseButMethodInProto;
@end

@protocol SubProto
- (id)propertyInBaseButMethodInProto;
@property id methodInBaseButPropertyInProto;
@end

@interface Sub : Base <SubProto>
@property id methodInBase;
- (id)propertyInBase;

- (void)setMethodPairInBase:(id)value;

- (id)getterOnlyInBase;
- (void)setGetterOnlyInBase:(id)value;

- (id)setterOnlyInBase;
@end
