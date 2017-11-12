// Swift 3 sees the ObjC class NSRuncibleSpoon as the class, and uses methods
// with type signatures involving NSRuncibleSpoon to conform to protocols
// across the language boundary. Swift 4 sees the type as bridged to
// a RuncibleSpoon value type, but still needs to be able to use conformances
// declared by Swift 3.

@import Foundation;

@interface NSRuncibleSpoon: NSObject
@end

@interface SomeObjCClass: NSObject
- (instancetype _Nonnull)initWithSomeSwiftInitRequirement:(NSRuncibleSpoon* _Nonnull)s;
- (void)someSwiftMethodRequirement:(NSRuncibleSpoon* _Nonnull)s;
@property NSRuncibleSpoon * _Nonnull someSwiftPropertyRequirement;
@end

@protocol SomeObjCProtocol
- (instancetype _Nonnull)initWithSomeObjCInitRequirement:(NSRuncibleSpoon * _Nonnull)string;
- (void)someObjCMethodRequirement:(NSRuncibleSpoon * _Nonnull)string;
@property NSRuncibleSpoon * _Nonnull someObjCPropertyRequirement;
@end


