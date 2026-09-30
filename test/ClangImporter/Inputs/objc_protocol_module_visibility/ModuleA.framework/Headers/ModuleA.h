#import <objc/NSObject.h>

@protocol RedeclaredInModuleA <NSObject>
@end

@protocol ForwardRedeclaredInModuleA;

@interface RedeclaredClassInModuleA : NSObject
@end

@class ForwardRedeclaredClassInModuleA;
