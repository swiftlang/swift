#ifndef TEST_INTEROP_CXX_OPERATORS_MOVE_ONLY_OPS_H
#define TEST_INTEROP_CXX_OPERATORS_MOVE_ONLY_OPS_H

#include <memory>

struct Copyable {
    int x;
};

struct NonCopyable {
    inline NonCopyable(int x) : x(x) {}
    inline NonCopyable(const NonCopyable &) = delete;
    inline NonCopyable(NonCopyable &&other) : x(other.x) { other.x = 0; }

    inline int method(int y) const { return x * y; }
    inline int mutMethod(int y) {
      x = y;
      return y;
    }

    int x;
};

#define NONCOPYABLE_HOLDER_WRAPPER(Name) \
private: \
NonCopyable x; \
public: \
inline Name(int x) : x(x) {} \
inline Name(const Name &) = delete; \
inline Name(Name &&other) : x(std::move(other.x)) {}

class NonCopyableHolderConstDeref {
    NONCOPYABLE_HOLDER_WRAPPER(NonCopyableHolderConstDeref)

    inline const NonCopyable & operator *() const { return x; }
};

#endif // TEST_INTEROP_CXX_OPERATORS_MOVE_ONLY_OPS_H
