#include "Runtime/Concurrency.h"
#include "Concurrency/Actor.h"
#include "Concurrency/Task.h"

#include "swift/Runtime/Atomic.h"
#include "swift/Runtime/Casting.h"
#include "Runtime/Threading/Mutex.h"
#include "Runtime/Threading/Once.h"
#include "Runtime/Threading/ThreadLocal.h"
#include "Runtime/Threading/ThreadLocalStorage.h"

#include <atomic>
#include <new>

using namespace swift;

namespace {

/// An extremely silly class which exists to make pointer
/// default-initialization constexpr.
template <class T> struct Pointer {
  T *Value;
  constexpr Pointer() : Value(nullptr) {}
  constexpr Pointer(T *value) : Value(value) {}
  operator T *() const { return Value; }
  T *operator->() const { return Value; }
};


class ActiveTask {
  /// A thread-local variable pointing to the active tracking
  /// information about the current thread, if any.
  static SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(Pointer<AsyncTask>, Value,
                                            SWIFT_CONCURRENCY_TASK_KEY);

public:
  static void set(AsyncTask *task) { Value.set(task); }
  static AsyncTask *get() { return Value.get(); }
};

/// Define the thread-locals.
SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(
  Pointer<AsyncTask>,
  ActiveTask::Value,
  SWIFT_CONCURRENCY_TASK_KEY);


} // namespace
