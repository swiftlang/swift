// This file is meant to be used with the mock SDK.
#import <Foundation.h>

__attribute__((objc_non_runtime_protocol))
@protocol NonRuntimeBridging
@end

@protocol RegularProtocol
@end

// A second non-runtime protocol for testing compositions
__attribute__((objc_non_runtime_protocol))
@protocol AnotherNonRuntimeProtocol
@end
