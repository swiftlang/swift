@import Foundation;

@interface InternalObjCClass : NSObject

- (void)methodFromHeader1:(int)param;

@end

@interface ObjCPropertyTest : NSObject

@property (readonly) int prop1;
@property (readwrite) int prop2;

- (instancetype)init;

- (void)doSomething;

@end
