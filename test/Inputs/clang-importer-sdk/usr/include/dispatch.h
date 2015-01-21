typedef struct dispatch_queue_t {} dispatch_queue_t;
typedef void (^dispatch_block_t)(void);
typedef struct dispatch_object_t {} dispatch_object_t;

dispatch_queue_t dispatch_get_current_queue(void);
void dispatch_async(dispatch_queue_t q, dispatch_block_t);

void dispatch_sync(dispatch_queue_t q, 
                   __attribute__((noescape)) dispatch_block_t);

void dispatch_retain(dispatch_object_t object);
void dispatch_release(dispatch_object_t object);
