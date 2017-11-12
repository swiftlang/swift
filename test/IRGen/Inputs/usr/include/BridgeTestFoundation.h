@import ObjectiveC;

@interface NSString : NSObject
- (NSString*)uppercaseString;
@end

@interface NSMutableString : NSString
@end

@interface NSArray<ObjectType> : NSObject
@end

@interface NSMutableArray<ObjectType> : NSArray<ObjectType>
@end

@interface NSDictionary<KeyType, ValueType> : NSObject
@end

@interface NSSet<ObjectType> : NSObject
@end

@interface NSMutableSet<ObjectType> : NSSet<ObjectType>
@end

@interface NSNumber : NSObject
@end

@interface NSNotification : NSObject
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

@interface NSData: NSObject <NSCopying>
@end

typedef struct __CGImage *CGImageRef;

__attribute__((availability(macosx,introduced=10.51)))
@interface NSUserNotificationAction : NSObject
@end

__attribute__((availability(macosx,introduced=10.51)))
void future_function_should_be_weak();

extern int weak_variable __attribute__((weak_import));

@interface NSError : NSObject

@property NSInteger code;
@property NSString *domain;
@property NSDictionary *userInfo;

@end

typedef NSString *_Nonnull NSNotificationName
    __attribute((swift_newtype(struct)));
