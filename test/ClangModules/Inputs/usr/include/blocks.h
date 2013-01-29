@import ObjectiveC;

typedef struct dispatch_queue_t {} dispatch_queue_t;

dispatch_queue_t dispatch_get_current_queue(void);
void dispatch_async(dispatch_queue_t q, void (^f)(void));

@interface NSString : NSObject

- (void)enumerateLinesUsingBlock:(void (^)(NSString *line)) f;
// FIXME: The importer drops this.
//- (void)enumerateLinesUsingBlock:(void (^)(NSString *line, BOOL *b)) f;

@end
