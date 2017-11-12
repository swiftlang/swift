// This file is meant to be used with the mock SDK, not the real one.
#import <Foundation.h>

#define SWIFT_NAME(x) __attribute__((swift_name(#x)))

typedef NSString *ABCStringAlias SWIFT_NAME(ZZStringAlias);
struct ABCPoint {
  int x;
  int y;
} SWIFT_NAME(ZZPoint);
enum ABCAlignment {
  ABCAlignmentLeft,
  ABCAlignmentRight
} SWIFT_NAME(ZZAlignment);

SWIFT_NAME(ZZClass)
@interface ABCClass : NSObject
@end

SWIFT_NAME(ZZProto)
@protocol ABCProto
@end
