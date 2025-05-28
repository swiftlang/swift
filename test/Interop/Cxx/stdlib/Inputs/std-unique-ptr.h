#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNIQUE_PTR_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNIQUE_PTR_H

#include <memory>
#include <string>

struct NonCopyable {
    NonCopyable(int x) : x(x) {}
    NonCopyable(const NonCopyable &) = delete;
    NonCopyable(NonCopyable &&other) : x(other.x) { other.x = -123; }

    int method(int y) const { return x * y; }
    int mutMethod(int y) {
      x = y;
      return y;
    }

    int x;
};

struct NonCopyableDerived: public NonCopyable {
    NonCopyableDerived(int x) : NonCopyable(x) {}
};


inline std::shared_ptr<NonCopyable> getNonCopyableSharedPtr() { return std::make_shared<NonCopyableDerived>(42); }
inline std::unique_ptr<NonCopyable> getNonCopyableUniquePtr() { return std::make_unique<NonCopyableDerived>(42); }

std::unique_ptr<int> makeInt() {
  return std::make_unique<int>(42);
}

std::unique_ptr<std::string> makeString() {
  return std::make_unique<std::string>("Unique string");
}

std::unique_ptr<int[]> makeArray() {
  int *array = new int[3];
  array[0] = 1;
  array[1] = 2;
  array[2] = 3;
  return std::unique_ptr<int[]>(array);
}

static bool dtorCalled = false;
struct HasDtor {
  HasDtor() = default;
#if __is_target_os(windows)
  // On windows, force this type to be address-only.
  HasDtor(const HasDtor &other);
#endif
  ~HasDtor() {
    dtorCalled = true;
  }
private:
  int x;
};

std::unique_ptr<HasDtor> makeDtor() {
  return std::make_unique<HasDtor>();
}

std::shared_ptr<int> makeIntShared() { return std::make_unique<int>(42); }

std::shared_ptr<std::string> makeStringShared() {
  return std::make_unique<std::string>("Shared string");
}

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNIQUE_PTR_H
