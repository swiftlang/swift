#include <stdint.h>

#if __has_feature(ptrauth_calls)
#include <ptrauth.h>
#endif

uintptr_t swift_task_getCurrent();

uintptr_t getCurrentTaskShim() { return swift_task_getCurrent(); }

// The unsigned function pointer of a Swift '() -> ()' function value, to
// compare it with what the reflection library reports for a task. Swift
// passes the function value as its function pointer and its context
__attribute__((swiftcall)) uintptr_t
getFunctionPointerShim(void *function, void *context) {
#if __has_feature(ptrauth_calls)
  function = ptrauth_strip(function, ptrauth_key_function_pointer);
#endif
  return (uintptr_t)function;
}
