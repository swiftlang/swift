@import Foundation;

@interface ObjCClass : NSObject

- (void)methodFromHeader1:(int)param;
- (void)methodFromHeader2:(int)param;

@property (readwrite) int propertyFromHeader1;
@property (readwrite) int propertyFromHeader2;

@end

@interface ObjCClass ()

- (void)extensionMethodFromHeader1:(int)param;
- (void)extensionMethodFromHeader2:(int)param;

@property (readwrite) int extensionPropertyFromHeader1;
@property (readwrite) int extensionPropertyFromHeader2;

@end
