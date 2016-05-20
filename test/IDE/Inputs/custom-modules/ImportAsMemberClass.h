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

@interface UnavailableDefaultInit : NSObject
-(instancetype)init __attribute__((availability(swift,unavailable)));
@end

@interface UnavailableDefaultInitSub : UnavailableDefaultInit
@end

__attribute__((swift_name("UnavailableDefaultInit.init()")))
UnavailableDefaultInit * _Nonnull MakeUnavailableDefaultInit(void);

__attribute__((swift_name("UnavailableDefaultInitSub.init()")))
UnavailableDefaultInitSub * _Nonnull MakeUnavailableDefaultInitSub(void);

#endif
