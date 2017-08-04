@import Foundation;

@interface ObjCFoo: NSObject
@end

@protocol ObjCProto <NSObject>

- (void)nonoptionalMethod;

@optional
- (void)optionalMethod;

@required
- (void)nonoptionalMethod2;

@end
