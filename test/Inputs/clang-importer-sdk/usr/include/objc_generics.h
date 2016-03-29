#import <Foundation.h>

@interface GenericClass<T> : NSObject
- (id)initWithThing:(T)thing;
- (void)dealloc;
- (__nullable T)thing;
- (int)count;
+ (__nullable T)classThing;
- (__nonnull NSArray<T> *)arrayOfThings;

@property (nonatomic) __nullable T propertyThing;
@end

@interface GenericClass<T> (Private)
- (__nullable T)otherThing;
+ (__nullable T)otherClassThing;
@end

void takeGenericClass(__nullable GenericClass<NSString *> *thing);

@interface GenericSubclass<T> : GenericClass<T>
@end

@protocol Pettable
@end

@interface Animal : NSObject
@end

@interface PettableContainer<T: id<Pettable>> : NSObject
@end

@interface AnimalContainer<T: Animal *> : NSObject
@end

@interface PettableAnimalContainer<T: Animal<Pettable> *> : NSObject
@end

@interface TestConstrainedTypeParam<T> : NSObject
- (void)doThing:(__nonnull T<Pettable>)thing;
@end

