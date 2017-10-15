#include "Foundation.h"

@interface NullableSubscript : NSObject
- (nullable NSString*) objectForKeyedSubscript: (nonnull id) key;
- (void) setObject: (nullable NSString*) obj forKeyedSubscript: (nonnull id) key;
@end

@interface NullproneSubscript : NSObject
- (null_unspecified NSString*) objectForKeyedSubscript: (nonnull id) key;
- (void) setObject: (null_unspecified NSString*) obj forKeyedSubscript: (nonnull id) key;
@end

@interface NonnullSubscript : NSObject
- (nonnull NSString*) objectForKeyedSubscript: (nonnull id) key;
- (void) setObject: (nonnull NSString*) obj forKeyedSubscript: (nonnull id) key;
@end

