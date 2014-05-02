@import Foundation;

typedef struct dispatch_queue_t {} dispatch_queue_t;
typedef void (^dispatch_block_t)(void);

dispatch_queue_t dispatch_get_current_queue(void);
void dispatch_async(dispatch_queue_t q, dispatch_block_t);

@interface NSString ()

- (void)enumerateLinesUsingBlock:(void (^)(NSString *line)) f;
// FIXME: The importer drops this.
//- (void)enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *b)) f;

@end
