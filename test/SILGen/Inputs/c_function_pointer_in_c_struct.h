typedef void (*_Nonnull Callback)();

struct opaqueStruct;

typedef struct {
    void (*_Nonnull voidReturning)();
    void *_Nonnull (*_Nonnull voidPointerReturning)();
    char *_Nonnull (*_Nonnull pointerReturning)();
    const char *_Nonnull (*_Nonnull constPointerReturning)();
    struct opaqueStruct *_Nonnull (*_Nonnull opaquePointerReturning)();
} SomeCallbacks;

void voidReturning();
void *_Nonnull voidPointerReturning();
char *_Nonnull pointerReturning();
const char *_Nonnull constPointerReturning();
struct opaqueStruct *_Nonnull opaquePointerReturning();
