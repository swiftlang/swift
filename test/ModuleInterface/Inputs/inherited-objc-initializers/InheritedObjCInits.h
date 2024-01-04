#import <Foundation/Foundation.h>

NS_SWIFT_UNAVAILABLE("unavailable")
@interface UnavailableInSwift : NSObject
@end

@interface HasAvailableInit : NSObject
- (nonnull instancetype)initWithUnavailable:(nonnull UnavailableInSwift *)unavailable;
@end

@interface HasUnavailableInit : NSObject
- (nonnull instancetype)initWithUnavailable:(nonnull UnavailableInSwift *)unavailable NS_SWIFT_UNAVAILABLE("unavailable");
@end

