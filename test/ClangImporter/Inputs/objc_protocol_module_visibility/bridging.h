#import <objc/NSObject.h>

@protocol RedeclaredInModuleA <NSObject>
@end

@protocol ForwardRedeclaredInModuleA <NSObject>
@end

@interface RedeclaredClassInModuleA : NSObject
@end

@interface ForwardRedeclaredClassInModuleA : NSObject
@end

@import ModuleA;
