typedef signed char BOOL;
@class NSString;

@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;

- (NSString*) description;

@property NSString *stringProperty;

@end

@interface NSString : NSObject

- (NSString*)uppercaseString;

@end

@interface Foo

- (NSString*) foo;
- (void) setFoo: (NSString*)s;

- (BOOL) zim;
- (void) setZim: (BOOL)b;

@end

NSString *bar(void);
void setBar(NSString *s);

