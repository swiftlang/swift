__attribute__((objc_root_class))
@interface RootObject
- (instancetype) init;
@end

@protocol RootProtocol
@optional
- (void) rootObject:(RootObject *)root willChangeValue:(void *)value;
- (void) rootObject:(RootObject *)root didChangeValue:(void *)value;
@end

@protocol ReactiveRootProtocol <RootProtocol>
@end

