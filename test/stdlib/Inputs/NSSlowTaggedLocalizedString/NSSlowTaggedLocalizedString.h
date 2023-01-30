#import <objc/objc.h>
#import <objc/NSObject.h>


@interface NSSlowTaggedLocalizedString : NSObject

+ (instancetype) createTestString;
+ (void) setContents: (const char *)newContents;

@end
