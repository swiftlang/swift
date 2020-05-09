@import Foundation;

#define kAliasErrorDomain "com.acme.AliasErrorDomain"

extern NSString *const __nonnull AliasErrorDomain;

typedef NS_ENUM(NSInteger, AliasError) {
    AliasErrorRealName = 1,
    AliasErrorFakeName = 1,
};
