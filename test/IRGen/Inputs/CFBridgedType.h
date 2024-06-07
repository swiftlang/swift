#define CF_BRIDGED_TYPE(T) __attribute__((objc_bridge(T)))

typedef struct CF_BRIDGED_TYPE(id) __CFBridgedType *CFBridgedTypeRef;

__attribute__((cf_audited_transfer))
CFBridgedTypeRef returnsACFBridgedType(void);
