@import Foundation;

struct opaqueStruct;

@interface BlockProperty

@property (strong, nonnull) void (^readWriteBlock)();
@property (readonly, nonnull) void (^readOnlyBlock)();
@property (readonly, nonnull) SEL selector;

- (void)voidReturning;
- (void *_Nonnull)voidPointerReturning;
- (struct opaqueStruct *_Nonnull)opaquePointerReturning;
- (char *_Nonnull)pointerReturning;
- (const char *_Nonnull)constPointerReturning;
- (id _Nonnull)idReturning;
- (SEL _Nonnull)selectorReturning;

- (BlockProperty *_Nonnull)objectReturning;

- (id _Nonnull)objectForKeyedSubscript:(id _Nonnull)key;

@end
