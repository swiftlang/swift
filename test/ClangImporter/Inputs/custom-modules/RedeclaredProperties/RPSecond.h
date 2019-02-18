@interface RPFoo ()
@property (readwrite, nullable) int *nonnullToNullable;
@property (readwrite, nonnull) int *nullableToNonnull;
@property (readwrite, nonnull) RPFoo *typeChangeMoreSpecific;
@property (readwrite, nonnull) id typeChangeMoreGeneral;
@property (readwrite, nullable) id accessorInProto;
@end

@interface RPSub : RPBase
@property (readwrite, nonatomic, nullable) id accessorInProto;
@end
