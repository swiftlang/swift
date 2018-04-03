#ifndef IMPORT_AS_MEMBER_CLASS_H
#define IMPORT_AS_MEMBER_CLASS_H

@import Foundation;

typedef NS_OPTIONS(NSInteger, IAMSomeClassOptions) {
  IAMSomeClassFuzzyDice = 0x01,
  IAMSomeClassSpoiler = 0x02
} __attribute__((swift_name("SomeClass.Options")));

__attribute__((swift_name("SomeClass")))
@interface IAMSomeClass : NSObject
@end

__attribute__((swift_name("SomeClass.init(value:)")))
IAMSomeClass * _Nonnull MakeIAMSomeClass(double x);

__attribute__((swift_name("SomeClass.applyOptions(self:_:)")))
void IAMSomeClassApplyOptions(IAMSomeClass * _Nonnull someClass, 
                              IAMSomeClassOptions options);

__attribute__((swift_name("SomeClass.doIt(self:)")))
void IAMSomeClassDoIt(IAMSomeClass * _Nonnull someClass);

@interface UnavailableDefaultInit : NSObject
-(instancetype)init __attribute__((availability(swift,unavailable)));
@end

@interface UnavailableDefaultInitSub : UnavailableDefaultInit
@end

__attribute__((swift_name("UnavailableDefaultInit.init()")))
UnavailableDefaultInit * _Nonnull MakeUnavailableDefaultInit(void);

__attribute__((swift_name("UnavailableDefaultInitSub.init()")))
UnavailableDefaultInitSub * _Nonnull MakeUnavailableDefaultInitSub(void);

#pragma clang assume_nonnull begin

extern NSString * PKPandaCutenessFactor __attribute__((swift_name("Panda.cutenessFactor")));
extern NSString * _Nullable PKPandaCuddlynessFactor __attribute__((swift_name("Panda.cuddlynessFactor")));

__attribute__((swift_name("Panda")))
@interface PKPanda : NSObject
@end

typedef NSString *IncompleteImportTargetName __attribute__((swift_wrapper(struct)));

@interface IncompleteImportTarget : NSObject
@end

#pragma clang assume_nonnull end

#endif
