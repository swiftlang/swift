#import <Foundation.h>
@import Dispatch;

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
