#import "ObjectiveC.h"

@interface NSString : NSObject<NSCopying>

- (NSString*)uppercaseString;
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

- (NSString*) foo;
- (void) setFoo: (NSString*)s;

- (BOOL) zim;
- (void) setZim: (BOOL)b;

- (_Bool) zang;
- (void) setZang: (_Bool)b;

@end

NSString *bar(void);
void setBar(NSString *s);

NSString *NSStringFromString(NSString *s);
NSString *NSStringFromClass(Class c);

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)

BOOL getBOOL(void);
_Bool getBool(void);


void useBOOL(BOOL x);
void useBool(_Bool x);
