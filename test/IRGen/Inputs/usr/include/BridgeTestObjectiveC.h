@class NSString;

typedef struct _NSZone NSZone;

typedef long NSInteger;
typedef unsigned long NSUInteger;

@protocol NSCopying
- copyWithZone:(nullable NSZone*)z;
@end

@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;
+ (void) initialize;

@property (readonly, copy) NSString *description;
@property (readonly) NSUInteger hash;
@end
