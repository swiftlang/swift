#import <Foundation.h>
#include <stdint.h>

#define _CF_TYPED_ENUM __attribute__((swift_wrapper(enum)))
#define NS_STRING_ENUM _CF_TYPED_ENUM
#define NS_SWIFT_NAME(Name) __attribute__((swift_name(#Name)))

typedef NSString * GenericOption NS_STRING_ENUM;

GenericOption const GenericOptionMultithreaded NS_SWIFT_NAME(multithreaded);


@interface GenericClass<T> : NSObject
- (id)initWithThing:(T)thing;
- (id)initWithArrayOfThings:(NSArray<T> *_Nonnull)things;
- (id)initWithOptions:(nullable NSDictionary<GenericOption, id> *)options;
- (void)dealloc;
- (_Nullable T)thing;
- (int)count;
+ (_Nullable T)classThing;
- (_Nonnull NSArray<T> *)arrayOfThings;
- (void)setArrayOfThings:(NSArray<T> *_Nonnull)things;

- (T _Nonnull)objectAtIndexedSubscript:(uint16_t)i;
- (void)setObject:(T _Nonnull)object atIndexedSubscript:(uint16_t)i;

- (void)performBlockOnThings:(T _Nonnull (^_Nonnull)(T _Nonnull))block;
- (T _Nonnull (^_Nonnull)(T _Nonnull))blockForPerformingOnThings;

@property(nonatomic) _Nullable T propertyThing;
@property(nonatomic) _Nullable NSArray<T> *propertyArrayOfThings;
@end

@interface GenericClass<T>(Private)
- (_Nullable T)otherThing;
+ (_Nullable T)otherClassThing;
@end

void takeGenericClass(_Nullable GenericClass<NSString *> *thing);

@interface GenericSubclass<T> : GenericClass<T>
@end

@protocol Pettable
- (nonnull instancetype)initWithFur:(nonnull id)fur;
- (nonnull instancetype)other;
+ (nonnull instancetype)adopt;
- (void)pet;
- (void)petWith:(nonnull id <Pettable>)other;

@property (nonatomic, class) _Nonnull id<Pettable> needingMostPets;

@end

@interface Animal : NSObject
- (nonnull instancetype)initWithNoise:(nonnull id)noise;
- (nonnull instancetype)another;
+ (nonnull instancetype)create;

- (void)eat:(Animal*)prey;

@property (nonatomic, readonly) Animal *_Nonnull buddy;

@property (nonatomic, class) Animal *_Nonnull apexPredator;

- (Animal *_Nonnull)objectAtIndexedSubscript:(NSInteger)i;
- (void)setObject:(Animal *_Nonnull)x atIndexedSubscript:(NSInteger)i;
@end

@interface PettableOverextendedMetaphor: NSObject <Pettable>
@end

@protocol Fungible
@end

@interface FungibleContainer<T : id<Fungible>> : NSObject
@end

@interface PettableContainer<T : id<Pettable>> : NSObject
@end

@interface AnimalContainer<T : Animal *> : NSObject
@end

@interface PettableAnimalContainer<T : Animal<Pettable> *> : NSObject
@end

@interface FungibleAnimalContainer<T : Animal<Fungible> *> : NSObject
@end

@interface TestConstrainedTypeParam<T> : NSObject
- (void)doThing:(_Nonnull T<Pettable>)thing;
@end

typedef id <Fungible> FungibleObject;

@interface Panda

// Unqualified reference to generic type

+ (AnimalContainer *)getContainer;
+ (FungibleAnimalContainer *)getFungibleContainer;

@end

@interface First<__covariant T> : NSObject
@end

@interface Second<__covariant T> : First<T>
@end

@class Third;

@interface Third : Second<Third *>
@end

typedef void (^ _Nonnull BlockPointerType)(void);

@interface HasBlockArray : NSObject
- (NSArray<BlockPointerType> * _Nonnull)blockArray;
- (BlockPointerType)blockPointerType;
@end
