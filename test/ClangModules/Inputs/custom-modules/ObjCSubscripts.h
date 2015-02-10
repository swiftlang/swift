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
