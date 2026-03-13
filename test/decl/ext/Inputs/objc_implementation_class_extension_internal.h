#import "objc_implementation_class_extension.h"

@interface ObjCClass ()

- (void)otherModuleExtensionMethodFromHeader1:(int)param;
- (void)otherModuleExtensionMethodFromHeader2:(int)param;

@property (readwrite) int otherModuleExtensionPropertyFromHeader1;
@property (readwrite) int otherModuleExtensionPropertyFromHeader2;

@end
