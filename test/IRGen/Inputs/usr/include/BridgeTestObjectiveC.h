@class NSString;

@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;
+ (void) initialize;

@property (readonly, copy) NSString *description;
@end

typedef long NSInteger;
