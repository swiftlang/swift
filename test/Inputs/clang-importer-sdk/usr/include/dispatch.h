#import <objc/NSObject.h>

@protocol OS_dispatch_object <NSObject>
@end
@interface OS_dispatch_object: NSObject <OS_dispatch_object>
@end
typedef OS_dispatch_object *dispatch_object_t;

@protocol OS_dispatch_queue <OS_dispatch_object>
@end
@interface OS_dispatch_queue: OS_dispatch_object <OS_dispatch_object>
@end
typedef OS_dispatch_queue *dispatch_queue_t;

@protocol OS_dispatch_source <OS_dispatch_object>
@end
@interface OS_dispatch_source: OS_dispatch_object <OS_dispatch_object>
@end
typedef OS_dispatch_source *dispatch_source_t;

typedef void (^dispatch_block_t)(void);

dispatch_queue_t dispatch_get_current_queue(void);
void dispatch_async(dispatch_queue_t q, dispatch_block_t) __attribute__((nonnull));

void dispatch_sync(dispatch_queue_t q, 
                   __attribute__((noescape)) dispatch_block_t) __attribute__((nonnull));

void dispatch_retain(dispatch_object_t object) __attribute__((nonnull));
void dispatch_release(dispatch_object_t object) __attribute__((nonnull));
