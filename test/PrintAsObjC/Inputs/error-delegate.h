// This file is meant to be used with the mock SDK, not the real one.
#import <Foundation.h>

@protocol ABCErrorProtocol <NSObject>

- (void)didFail:(NSError * _Nonnull)error;
- (void)didFailOptional:(NSError * _Nullable)error;

@end
