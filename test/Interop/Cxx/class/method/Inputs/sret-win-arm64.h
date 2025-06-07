#ifndef TEST_INTEROP_CXX_CLASS_METHOD_SRET_WIN_ARM64_H
#define TEST_INTEROP_CXX_CLASS_METHOD_SRET_WIN_ARM64_H

#include <stdint.h>

namespace llvm {

template<typename T>
class ArrayRef {
public:
   const T *Data = nullptr;
   size_t Length = 0;
};

} // namespace llvm

namespace swift {

class Type {
};

class SubstitutionMap {
private:
  void *storage = nullptr;

public:
  llvm::ArrayRef<Type> getReplacementTypes() const;
};

} // namespace swift

class BridgedArrayRef {
public:
  const void * Data;
  size_t Length;

  BridgedArrayRef() : Data(nullptr), Length(0) {}

#ifdef USED_IN_CPP_SOURCE
  template <typename T>
  BridgedArrayRef(llvm::ArrayRef<T> arr)
      : Data(arr.Data), Length(arr.Length) {}

  template <typename T>
  llvm::ArrayRef<T> unbridged() const {
    return {static_cast<const T *>(Data), Length};
  }
#endif
};

struct BridgedSubstitutionMap {
  uint64_t storage[1];

#ifdef USED_IN_CPP_SOURCE
  BridgedSubstitutionMap(swift::SubstitutionMap map) {
    *reinterpret_cast<swift::SubstitutionMap *>(&storage) = map;
  }
  swift::SubstitutionMap unbridged() const {
    return *reinterpret_cast<const swift::SubstitutionMap *>(&storage);
  }
#endif

  BridgedSubstitutionMap() {}
};

struct BridgedTypeArray {
  BridgedArrayRef typeArray;

#ifdef AFTER_FIX
   BridgedTypeArray() : typeArray() {}
#endif

#ifdef USED_IN_CPP_SOURCE
  BridgedTypeArray(llvm::ArrayRef<swift::Type> types) : typeArray(types) {}

  llvm::ArrayRef<swift::Type> unbridged() const {
    return typeArray.unbridged<swift::Type>();
  }
#endif

  static BridgedTypeArray fromReplacementTypes(BridgedSubstitutionMap substMap);
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_SRET_WIN_ARM64_H
