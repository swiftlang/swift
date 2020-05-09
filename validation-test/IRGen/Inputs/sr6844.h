@import Foundation;

typedef NSObject *MyJSONKeyPath __attribute__((swift_wrapper(struct)));

@protocol MyJSONSerializing <NSObject>
@property (copy, readonly, nullable) NSDictionary<NSString *, MyJSONKeyPath> *JSONKeyPathsByPropertyKey NS_SWIFT_NAME(JSONKeyPathsByPropertyKey);
@end

@interface MyJSONAdapter : NSObject
- (nonnull instancetype)init;
- (nonnull __kindof id<MyJSONSerializing>)model NS_REFINED_FOR_SWIFT;
@end
