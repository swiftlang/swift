@import Foundation;

@protocol BaseProto
@end

@protocol SubProto <BaseProto>
@end

@interface NonGenericType: NSObject <SubProto>
@end

@interface GenericType<Element>: NSObject <SubProto>
@end
