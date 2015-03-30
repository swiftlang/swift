@import ObjectiveC;

@interface NSString : NSObject
- (NSString*)uppercaseString;
@end

@interface NSMutableString : NSString
@end

@interface NSArray : NSObject
@end

@interface NSMutableArray : NSObject
@end

@interface NSDictionary : NSObject
@end

@interface NSSet : NSObject
@end

@interface NSMutableSet : NSObject
@end

@interface NSNumber : NSObject
@end

@interface Foo

- (NSString*) foo;
- (void) setFoo: (NSString*)s;

@end

NSString *bar(int);
void setBar(NSString *s);

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)

@interface NSManagedObject: NSObject
@end

typedef struct NSZone NSZone;

@protocol NSCopying
- copyWithZone:(NSZone*)z;
@end

@interface NSData: NSObject <NSCopying>
@end

typedef struct __CGImage *CGImageRef;

__attribute__((availability(macosx,introduced=10.10)))
@interface NSUserNotificationAction : NSObject
@end

extern int weak_variable __attribute__((weak_import));

@interface NSError : NSObject

@property NSInteger code;
@property NSString *domain;
@property NSDictionary *userInfo;

@end
