@import ObjectiveC;

@interface MyObjCClass : NSObject
- (instancetype)init;
@end
#define ALIASED_OBJC_CLASS MyObjCClass

@protocol MyObjCProtocol
@end
#define ALIASED_OBJC_PROTOCOL MyObjCProtocol
