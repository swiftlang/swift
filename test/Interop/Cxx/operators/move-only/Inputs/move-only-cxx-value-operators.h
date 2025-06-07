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

class NonCopyableHolderPairedDeref {
    NONCOPYABLE_HOLDER_WRAPPER(NonCopyableHolderPairedDeref)

    inline const NonCopyable & operator *() const { return x; }
    inline NonCopyable & operator *() { return x; }
};

class NonCopyableHolderMutDeref {
    NONCOPYABLE_HOLDER_WRAPPER(NonCopyableHolderMutDeref)

    inline NonCopyable & operator *() { return x; }
};

class NonCopyableHolderValueConstDeref {
    NONCOPYABLE_HOLDER_WRAPPER(NonCopyableHolderValueConstDeref)

    inline NonCopyable operator *() const { return NonCopyable(x.x); }
};

class NonCopyableHolderValueMutDeref {
    NONCOPYABLE_HOLDER_WRAPPER(NonCopyableHolderValueMutDeref)

    inline NonCopyable operator *() { return NonCopyable(x.x); }
};

template<class T>
class OneDerived: public T {
public:
    OneDerived(int x) : T(x) {}
};

using NonCopyableHolderConstDerefDerivedDerived = OneDerived<OneDerived<NonCopyableHolderConstDeref>>;

using NonCopyableHolderPairedDerefDerivedDerived = OneDerived<OneDerived<NonCopyableHolderPairedDeref>>;

using NonCopyableHolderMutDerefDerivedDerived = OneDerived<OneDerived<NonCopyableHolderMutDeref>>;

using NonCopyableHolderValueConstDerefDerivedDerived = OneDerived<OneDerived<NonCopyableHolderValueConstDeref>>;

using NonCopyableHolderValueMutDerefDerivedDerived = OneDerived<OneDerived<NonCopyableHolderValueMutDeref>>;

#endif // TEST_INTEROP_CXX_OPERATORS_MOVE_ONLY_OPS_H
