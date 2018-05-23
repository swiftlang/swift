#import <objc_generics.h>
extern int ANTGlobalValue;

@interface NewType
@end
@interface OldType
@end

@protocol TypeWithMethod
  -(void) minusPrint;
  +(void) plusPrint;
  @property int PropertyA;
@end

@protocol ObjcProt
  -(void) ProtMemberFunc;
@end

typedef NSString * AnimalAttributeName NS_STRING_ENUM;

@interface AnimalStatusDescriptor
- (nonnull AnimalStatusDescriptor *)animalStatusDescriptorByAddingAttributes:(nonnull NSDictionary<AnimalAttributeName, id> *)attributes;
- (nonnull AnimalStatusDescriptor *)animalStatusDescriptorByAddingOptionalAttributes:(nullable NSDictionary<AnimalAttributeName, id> *)attributes;
- (nonnull AnimalStatusDescriptor *)animalStatusDescriptorByAddingAttributesArray:(nonnull NSArray<AnimalAttributeName> *)attributes;
- (nonnull AnimalStatusDescriptor *)animalStatusDescriptorByAddingOptionalAttributesArray:(nullable NSArray<AnimalAttributeName> *)attributes;
+ (nonnull AnimalStatusDescriptor *)animalStatusSingleOptionalAttribute:(nullable AnimalAttributeName)attributes;
+ (nonnull AnimalStatusDescriptor *)animalStatusSingleAttribute:(nonnull AnimalAttributeName)attributes;
@end

extern AnimalAttributeName globalAttributeName;

typedef NSString * CatAttributeName NS_STRING_ENUM;
