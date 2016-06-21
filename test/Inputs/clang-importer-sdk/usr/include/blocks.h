@import Foundation;
@import Dispatch;

@interface NSString ()

- (void)enumerateLinesUsingBlock:(nonnull __attribute__((noescape)) void (^)(__nonnull NSString *line)) f;
// FIXME: The importer drops this.
//- (void)enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *b)) f;

@end

typedef void (^my_block_t)(void);

my_block_t blockWithoutNullability();
my_block_t __nonnull blockWithNonnull();
my_block_t __null_unspecified blockWithNullUnspecified();
my_block_t __nullable blockWithNullable();

void accepts_block(my_block_t) __attribute__((nonnull));
void accepts_noescape_block(__attribute__((noescape)) my_block_t) __attribute__((nonnull));

