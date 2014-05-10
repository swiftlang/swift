#import "ObjectiveC.h"

@interface NSString : NSObject<NSCopying>

- (NSString*)uppercaseString;
- (id) copyWithZone: (void*)zone;

@end

@interface NSArray : NSObject

- (instancetype)initWithObjects:(const id *)objects count:(int)count;

@end

@interface NSDictionary : NSObject
@end

@interface Foo : NSObject

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
