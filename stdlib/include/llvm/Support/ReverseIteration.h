#ifndef LLVM_SUPPORT_REVERSEITERATION_H
#define LLVM_SUPPORT_REVERSEITERATION_H

#include "llvm/Config/abi-breaking.h"
#include "llvm/Support/PointerLikeTypeTraits.h"

inline namespace __swift { inline namespace __runtime {
namespace llvm {

template<class T = void *>
bool shouldReverseIterate() {
#if LLVM_ENABLE_REVERSE_ITERATION
  return detail::IsPointerLike<T>::value;
#else
  return false;
#endif
}

}
}} // namespace swift::runtime
#endif
