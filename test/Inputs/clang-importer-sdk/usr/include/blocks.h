@import Foundation;
@import Dispatch;

@interface NSString ()

- (void)enumerateLinesUsingBlock:(nonnull __attribute__((noescape)) void (^)(NSString *line)) f;
// FIXME: The importer drops this.
//- (void)enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *b)) f;

@end
