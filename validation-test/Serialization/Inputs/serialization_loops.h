@import Foundation;

typedef NS_ENUM(NSUInteger, FooKind) {
  FooKindA
};

@protocol BaseProto
@property FooKind kind;
@end

@interface Base : NSObject <BaseProto>
@end
