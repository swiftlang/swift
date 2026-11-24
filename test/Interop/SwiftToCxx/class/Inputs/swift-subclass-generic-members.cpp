#include "class.h"
#ifdef EXPLICIT_EXPOSURE
#include "useclass-exposed.h"
#else
#include "useclass.h"
#endif

#include <type_traits>
#include <utility>

static_assert(
    std::is_same<decltype(std::declval<UseClass::GenericMemberDerivedClass &>()
                              .getComputedProp()),
                 swift::Int>::value,
    "preserve imported getter");

swift::Int checkImportedMembers(UseClass::GenericMemberDerivedClass &value) {
  return value.getComputedProp() + value.novel().getValue();
}
