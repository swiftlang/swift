typedef signed char BOOL;
@class NSString;

@protocol NSCopying
- (id) copyWithZone: (void*)zone;
@end

@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;

- (NSString*) description;

@property NSString *stringProperty;

@end

@interface NSString : NSObject<NSCopying>

- (NSString*)uppercaseString;
- (id) copyWithZone: (void*)zone;

@end

@interface NSArray : NSObject
@end

@interface Foo

- (NSString*) foo;
- (void) setFoo: (NSString*)s;

- (BOOL) zim;
- (void) setZim: (BOOL)b;

@end

NSString *bar(void);
void setBar(NSString *s);

NSString *NSStringFromString(NSString *s);

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)
