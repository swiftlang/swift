@class NSString;

typedef struct _NSZone NSZone;

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
@end

typedef long NSInteger;
