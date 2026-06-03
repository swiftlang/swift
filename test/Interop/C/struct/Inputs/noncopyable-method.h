#include "swift/bridging"

// A ~Copyable C struct with a destroy operation.
typedef struct SWIFT_NONCOPYABLE_WITH_DESTROY(Handle_deinit) Handle {
  int value;
} Handle;

#ifdef __cplusplus
extern "C" {
#endif

void Handle_deinit(Handle handle);

// 'const Handle *self' is a borrow: on a ~Copyable type it must import as a
// borrowing method, else 'self' lowers to consuming 'in'.
int Handle_read(const Handle *self, int id) SWIFT_NAME(Handle.read(self:id:));

// Non-const 'Handle *self' must stay mutating.
void Handle_update(Handle *self, int id) SWIFT_NAME(Handle.update(self:id:));

#ifdef __cplusplus
}
#endif
