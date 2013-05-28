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

@end

NSString *bar(int);
void setBar(NSString *s);

