#include <coroutine>

struct CoroutineHandleHolder {
  std::coroutine_handle<> handle;
};

inline bool isSuspendNeverReady() {
  std::suspend_never s;
  return s.await_ready();
}
