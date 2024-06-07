#ifndef TEST_INTEROP_CXX_CLASS_MOVE_ONLY_VT_H
#define TEST_INTEROP_CXX_CLASS_MOVE_ONLY_VT_H

#include <memory>

struct Copyable {
    int x;
};

struct NonCopyable {
    inline NonCopyable(int x) : x(x) {}
    inline NonCopyable(const NonCopyable &) = delete;
    inline NonCopyable(NonCopyable &&other) : x(other.x) { other.x = -123; }

    inline int method(int y) const { return x * y; }
    inline int mutMethod(int y) {
      x = y;
      return y;
    }

    int x;
};

struct NonCopyableDerived: public NonCopyable {
    NonCopyableDerived(int x) : NonCopyable(x) {}
};

struct NonCopyableDerivedDerived: public NonCopyableDerived {
    NonCopyableDerivedDerived(int x) : NonCopyableDerived(x) {}
};

struct NonCopyableHolder {
    inline NonCopyableHolder(int x) : x(x) {}
    inline NonCopyableHolder(const NonCopyableHolder &) = delete;
    inline NonCopyableHolder(NonCopyableHolder &&other) : x(std::move(other.x)) {}

    inline NonCopyable &returnMutNonCopyableRef() { return x; }

    inline const NonCopyable &returnNonCopyableRef() const { return x; }

    NonCopyable x;
};

struct NonCopyableHolderDerived: NonCopyableHolder {
    inline NonCopyableHolderDerived(int x) : NonCopyableHolder(x) {}
};

struct NonCopyableHolderDerivedDerived: NonCopyableHolderDerived {
    inline NonCopyableHolderDerivedDerived(int x) : NonCopyableHolderDerived(x) {}

    inline int getActualX() const {
        return x.x;
    }
};

inline NonCopyable *getNonCopyablePtr() { return nullptr; }
inline NonCopyableDerived *getNonCopyableDerivedPtr() { return nullptr; }

#endif // TEST_INTEROP_CXX_CLASS_MOVE_ONLY_VT_H
