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
@end
