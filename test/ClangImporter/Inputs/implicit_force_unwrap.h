@import Foundation;

// No nullability annotations, so object pointers are imported as implicitly
// unwrapped optionals.
@interface IUOBase : NSObject
- (void)useObject:(NSObject *)object other:(NSObject *)other;
- (void)takeBlock:(void (^)(void))block;
- (NSObject *)produceObject;
- (NSObject *)objectForKeyedSubscript:(NSObject *)key;
- (instancetype)initWithObject:(NSObject *)object;
@end

@protocol IUOProto
- (void)requireUseObject:(NSObject *)object;
- (NSObject *)requireProduceObject;
@property NSObject *settableProp;
@property NSObject *inferredProp;
@property (readonly) NSObject *readOnlyProp;
@end
