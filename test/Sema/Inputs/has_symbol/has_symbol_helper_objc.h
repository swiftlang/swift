@import Foundation;

extern NSString *const StringConstant;

@interface ObjCClass : NSObject
@property int classMemberProperty;
@property(direct) int directClassMemberProperty;
- (void)classMemberMethod;
- (void)directClassMemberMethod __attribute__((objc_direct));
@end

@protocol ObjCProtocol <NSObject>
@property NSObject *protocolMemberProperty;
- (void)protocolMemberMethod;
@end
