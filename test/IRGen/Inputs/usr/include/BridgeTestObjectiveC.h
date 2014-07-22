@class NSString;

@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;

@property (readonly, copy) NSString *description;
@end
