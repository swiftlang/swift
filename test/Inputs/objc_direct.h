#import <Foundation/Foundation.h>

@interface Bar : NSObject
- (instancetype)initWithValue:(int)value __attribute__((objc_direct));
@property(direct) int directProperty;
- (int)objectAtIndexedSubscript:(int)i __attribute__((objc_direct));
- (void)setObject:(int)obj atIndexedSubscript:(int)i __attribute__((objc_direct));
- (NSString *)directMethod __attribute__((objc_direct));
+ (NSString *)directClassMethod __attribute__((objc_direct));
- (NSString *)directProtocolMethod __attribute__((objc_direct));
@end

__attribute__((objc_direct_members))
@interface Bar(CategoryName)
@property int directProperty2;
- (NSString *)directMethod2;
+ (NSString *)directClassMethod2;
@end
