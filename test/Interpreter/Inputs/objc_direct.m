#import "objc_direct.h"

@implementation Bar
- (instancetype)initWithValue:(int)value {
  printf("called %s with %d\n", __FUNCTION__, value);
  self = [super init];
  _directProperty = value;
  return self;
}
- (int)objectAtIndexedSubscript:(int)i {
  return 789;
}
- (void)setObject:(int)obj atIndexedSubscript:(int)i {}
- (NSString *)directMethod {
  return @"called directMethod";
}
+ (NSString *)directClassMethod {
  return @"called directClassMethod";
}
- (NSString *)directProtocolMethod {
  return @"called directProtocolMethod";
}
@end

@implementation Bar(CategoryName)
- (int)directProperty2 {
  return 456;
}
- (void)setDirectProperty2:(int)i {}
- (NSString *)directMethod2 {
  return @"called directMethod2";
}
+ (NSString *)directClassMethod2 {
  return @"called directClassMethod2";
}
@end
