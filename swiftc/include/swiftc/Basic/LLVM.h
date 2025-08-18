#ifndef SWIFTC_BASIC_LLVM_H
#define SWIFTC_BASIC_LLVM_H

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>

namespace swiftc {

// Import common LLVM types into our namespace
using llvm::ArrayRef;
using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;
using llvm::MutableArrayRef;
using llvm::SmallString;
using llvm::SmallVector;
using llvm::SmallVectorImpl;
using llvm::StringRef;

template<typename T>
using DenseMap = llvm::DenseMap<T, T>;

} // namespace swiftc

#endif // SWIFTC_BASIC_LLVM_H