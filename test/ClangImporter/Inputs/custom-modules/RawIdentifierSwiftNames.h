@import Foundation;
@import SwiftName;

#pragma clang assume_nonnull begin
@interface RawIdentifierTest : NSObject
- (void)methodWithRawIdentifier:(NSInteger)value
    SWIFT_NAME(`do something`(`with value `:));
@property(readonly) int rawIdentifierProp SWIFT_NAME(`raw prop`);
@end
#pragma clang assume_nonnull end

extern int test_raw SWIFT_NAME(`test raw`);
extern int global_raw SWIFT_NAME(`3global raw`);
extern int fooWithUnnecessaryBackticks SWIFT_NAME(`foo`);
