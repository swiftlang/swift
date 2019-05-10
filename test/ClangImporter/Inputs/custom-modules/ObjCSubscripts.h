@import ObjectiveC;
@import Foundation;

@interface KeySubscript1 : NSObject
- (id)objectForKeyedSubscript:(NSString *)subscript;
- (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
@end

@interface KeySubscript2 : NSObject
- (nullable id)objectForKeyedSubscript:(nonnull NSString *)subscript;
- (void)setObject:(nullable id)object forKeyedSubscript:(nonnull NSString *)key;
@end

@interface KeySubscript3 : NSObject
- (nullable NSString *)objectForKeyedSubscript:(nonnull NSString *)subscript;
- (void)setObject:(nullable NSString *)object forKeyedSubscript:(nonnull NSString *)key;
@end

@interface KeySubscript4 : NSObject
- (nullable NSString *)objectForKeyedSubscript:(nonnull NSArray *)subscript;
- (void)setObject:(nullable NSString *)object forKeyedSubscript:(nonnull NSArray *)key;
@end

@protocol KeySubscriptProto1
- (nullable NSString *)objectForKeyedSubscript:(nonnull NSString *)subscript;
@end

@protocol KeySubscriptProto2
- (NSString *)objectForKeyedSubscript:(NSString *)subscript;
- (void)setObject:(NSString *)object forKeyedSubscript:(NSString *)key;
@end


// rdar://problem/36033356 failed specifically when the base class was never
// subscripted, so please don't mention this class in the .swift file.
@interface KeySubscriptBase
- (id)objectForKeyedSubscript:(NSString *)subscript;
- (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
@end

@interface KeySubscriptOverrideGetter : KeySubscriptBase
- (id)objectForKeyedSubscript:(NSString *)subscript;
@end

@interface KeySubscriptOverrideSetter : KeySubscriptBase
- (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
@end

// rdar://problem/36033356 failed specifically when the base class was never
// subscripted, so please don't mention this class in the .swift file.
@interface KeySubscriptReversedBase
- (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
- (id)objectForKeyedSubscript:(NSString *)subscript;
@end

@interface KeySubscriptReversedOverrideGetter : KeySubscriptReversedBase
- (id)objectForKeyedSubscript:(NSString *)subscript;
@end

@interface KeySubscriptReversedOverrideSetter : KeySubscriptReversedBase
- (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
@end

@interface NoClassSubscript : NSObject
+ (id)objectAtIndexedSubscript:(int)i;
+ (void)setObject:(id)obj atIndexedSubscript:(int)i;
+ (id)objectForKeyedSubscript:(NSString *)subscript;
+ (void)setObject:(id)object forKeyedSubscript:(NSString *)key;
@end
