
#include <stdint.h>

uintptr_t swift_task_getCurrent();

uintptr_t getCurrentTaskShim() { return swift_task_getCurrent(); }
