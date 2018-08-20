@protocol RPProto
- (nullable id)accessorInProto;
@end

@interface RPFoo <RPProto>
@property (readonly, nonnull) int *nonnullToNullable;
@property (readonly, nullable) int *nullableToNonnull;
@property (readonly, nonnull) id typeChangeMoreSpecific;
@property (readonly, nonnull) RPFoo *typeChangeMoreGeneral;

@property (readonly, nonnull) id accessorRedeclaredAsNullable;
- (nullable id)accessorRedeclaredAsNullable;

- (nullable id)accessorDeclaredFirstAsNullable;
@property (readonly, nonnull) id accessorDeclaredFirstAsNullable;

@property (readonly, nullable) id accessorInProto;
@end

@interface RPBase <RPProto>
@property (readonly, nonatomic, nullable) id accessorInProto;
@end
