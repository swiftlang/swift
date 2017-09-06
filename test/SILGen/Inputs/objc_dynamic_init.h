@import Foundation;

@protocol ObjCInitProto

- initWithProto:(NSInteger)x;

@end

@interface ObjCBaseWithInitProto: NSObject <ObjCInitProto>

- initWithProto:(NSInteger)x NS_DESIGNATED_INITIALIZER;

@end
