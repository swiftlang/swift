// This file is meant to be included with modules turned off, compiled against
// the fake clang-importer-sdk.
#import <Foundation.h>

@interface Outer : NSObject
@end

__attribute__((swift_name("Outer.Inner")))
@interface InnerClass : NSObject
@end
