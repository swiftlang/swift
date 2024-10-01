
struct dispatch_queue_s;

extern struct dispatch_queue_s _dispatch_main_q;

static inline void *getDispatchMain() { return (void *)&_dispatch_main_q; }
