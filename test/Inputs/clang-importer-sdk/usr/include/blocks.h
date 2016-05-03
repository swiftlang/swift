@import Foundation;
@import Dispatch;

@interface NSString ()

- (void)enumerateLinesUsingBlock:(nonnull __attribute__((noescape)) void (^)(__nonnull NSString *line)) f;
// FIXME: The importer drops this.
//- (void)enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *b)) f;

@end

dispatch_block_t blockWithoutNullability();
dispatch_block_t __nonnull blockWithNonnull();
dispatch_block_t __null_unspecified blockWithNullUnspecified();
dispatch_block_t __nullable blockWithNullable();
