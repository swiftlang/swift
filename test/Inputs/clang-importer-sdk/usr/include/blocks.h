@import Foundation;
@import Dispatch;

@interface NSString ()

- (void)enumerateLinesUsingBlock:
    (nonnull __attribute__((noescape)) void (^)(_Nonnull NSString *line))f;
// FIXME: The importer drops this.
//- (void)enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *b)) f;

@end

typedef void (^my_block_t)(void);

my_block_t blockWithoutNullability();
my_block_t _Nonnull blockWithNonnull();
my_block_t _Null_unspecified blockWithNullUnspecified();
my_block_t _Nullable blockWithNullable();

void accepts_block(my_block_t) __attribute__((nonnull));
void accepts_noescape_block(__attribute__((noescape)) my_block_t) __attribute__((nonnull));

// Please see related tests in PrintAsObjC/imported-block-typedefs.swift.

