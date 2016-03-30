#import "ObjectiveC.h"
#import "dispatch.h"

#pragma clang assume_nonnull begin

@interface NSString : NSObject<NSCopying>

- (null_unspecified NSString*)uppercaseString;
- (id) copyWithZone: (nullable void*)zone;

@end

@interface NSArray<ObjectType> : NSObject

- (instancetype)initWithObjects:(const ObjectType *)objects count:(int)count;
- (instancetype)initWithArray:(NSArray<ObjectType>*)array;

- (nonnull ObjectType)objectAtIndexedSubscript:(NSInteger)i;

@end

@interface NSDictionary<KeyType, ValueType> : NSObject
@end

@interface NSSet<ObjectType> : NSObject
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

@interface NSDraggingItem

@property(copy, nullable) NSArray *__nonnull (^imageComponentsProvider)(void);

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

void nonnullStringBlockResult(NSString *_Nonnull (^block)(void));
void nonnullArrayBlockResult(NSArray *_Nonnull (^block)(void));
void nonnullDictionaryBlockResult(NSDictionary *_Nonnull (^block)(void));
void nonnullSetBlockResult(NSSet *_Nonnull (^block)(void));

void noescapeBlock(__attribute__((noescape)) void (^block)(void));
void escapeBlock(void (^block)(void));

void noescapeNonnullBlock(__attribute__((noescape)) void (^_Nonnull block)(void));
void escapeNonnullBlock(void (^_Nonnull block)(void));

void noescapeBlockAlias(__attribute__((noescape)) dispatch_block_t block);
void noescapeNonnullBlockAlias(__attribute__((noescape)) _Nonnull dispatch_block_t block);
void escapeBlockAlias(dispatch_block_t block);
