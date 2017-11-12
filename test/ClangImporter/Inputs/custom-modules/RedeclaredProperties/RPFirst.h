@interface RPFoo
@property (readonly, nonnull) int *nonnullToNullable;
@property (readonly, nullable) int *nullableToNonnull;
@property (readonly, nonnull) id typeChangeMoreSpecific;
@property (readonly, nonnull) RPFoo *typeChangeMoreGeneral;

@property (readonly, nonnull) id accessorRedeclaredAsNullable;
- (nullable id)accessorRedeclaredAsNullable;

- (nullable id)accessorDeclaredFirstAsNullable;
@property (readonly, nonnull) id accessorDeclaredFirstAsNullable;
@end
