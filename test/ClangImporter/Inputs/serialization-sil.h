@protocol Test
@optional
- (nonnull id)normalObject;
- (nonnull void *)innerPointer __attribute__((objc_returns_inner_pointer));
@property (nonnull) id normalObjectProp;
@property (nonnull) void *innerPointerProp __attribute__((objc_returns_inner_pointer));
@end
