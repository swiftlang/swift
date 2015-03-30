#import "ObjectiveC.h"

#pragma clang assume_nonnull begin

@interface NSString : NSObject<NSCopying>

- (__null_unspecified NSString*)uppercaseString;
- (id) copyWithZone: (void*)zone;

@end

@interface NSArray : NSObject

- (instancetype)initWithObjects:(const id *)objects count:(int)count;
- (instancetype)initWithArray:(NSArray*)array;
@end

@interface NSDictionary : NSObject
@end

@interface NSSet : NSObject
@end

@interface NSNumber : NSObject
@end

@interface Foo : NSObject

- (__null_unspecified NSString*) foo;
- (void) setFoo: (__null_unspecified NSString*)s;

- (BOOL) zim;
- (void) setZim: (BOOL)b;

- (_Bool) zang;
- (void) setZang: (_Bool)b;

@property int intProperty;

@end

@interface NSError : NSObject

@property NSInteger code;
@property NSString *domain;
@property NSDictionary *userInfo;

@end

__null_unspecified NSString *bar(void);
void setBar(__null_unspecified NSString *s);

__null_unspecified NSString *NSStringFromString(__null_unspecified NSString *s);
NSString *NSStringFromClass(Class c);

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)

BOOL getBOOL(void);
_Bool getBool(void);


void useBOOL(BOOL x);
void useBool(_Bool x);

#pragma clang assume_nonnull end

