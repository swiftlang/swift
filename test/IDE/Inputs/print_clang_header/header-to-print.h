#import <Foundation.h>
#import <CoreFoundation.h>
@import Dispatch;
#include "other-header.h"

#define MACRO_GOT_UNDEFINED 1
#undef MACRO_GOT_UNDEFINED
#define MY_MACRO 1

void doSomethingInHead(int arg);

@interface BaseInHead
- (void)doIt:(int)arg;
@end

/// Awesome name.
@interface SameName
@end
@protocol SameName
@end

@interface BaseInHead(SomeCategory)
-(void)doItInCategory;
@end

@protocol Superproto
-(void)lala;
@end

@class Cake;
struct Arkham;
@protocol Soul;

typedef struct __attribute__((objc_bridge(id))) __MyLittleCFType *MyLittleCFType;

@interface MaybeAvailable
-(void)method1 __attribute__((availability(macosx, introduced=10.1)));
-(void)method2 __attribute__((availability(macosx, introduced=10_1)));
-(void)method3 __attribute__((availability(macosx, deprecated=10_10)));
-(void)method4 __attribute__((availability(macosx, introduced=10_1, deprecated=10_10, obsoleted=10_11)));
@end
