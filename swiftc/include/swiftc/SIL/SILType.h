#ifndef SWIFTC_SIL_SILTYPE_H
#define SWIFTC_SIL_SILTYPE_H

#include "swiftc/Basic/LLVM.h"
#include <memory>

namespace swiftc {

class Type;

/// SIL type system - a lowered representation of Swift types.
class SILType {
public:
  enum class Category {
    Object,    // Value types
    Address    // Reference types
  };

private:
  std::unique_ptr<Type> CanonicalType;
  Category TypeCategory;

public:
  SILType(std::unique_ptr<Type> type, Category category)
      : CanonicalType(std::move(type)), TypeCategory(category) {}

  Type* getCanonicalType() const { return CanonicalType.get(); }
  Category getCategory() const { return TypeCategory; }

  bool isObject() const { return TypeCategory == Category::Object; }
  bool isAddress() const { return TypeCategory == Category::Address; }

  /// Get the address type for this object type.
  static std::unique_ptr<SILType> getAddressType(std::unique_ptr<Type> type) {
    return std::make_unique<SILType>(std::move(type), Category::Address);
  }

  /// Get the object type for this address type.
  static std::unique_ptr<SILType> getObjectType(std::unique_ptr<Type> type) {
    return std::make_unique<SILType>(std::move(type), Category::Object);
  }
};

} // namespace swiftc

#endif // SWIFTC_SIL_SILTYPE_H