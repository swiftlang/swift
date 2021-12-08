@import Foundation;

@interface ObjCClass<V> : NSObject

- (void)barWithBlock:(nullable void (^)(V _Nullable))block;

@end
