
#ifndef SWIFTC_BASIC_LLVM_H
#define SWIFTC_BASIC_LLVM_H
#include <string>
#include <vector>
#include <map>
namespace swiftc {
using StringRef = const std::string&;
template<typename T> using ArrayRef = const std::vector<T>&;
template<typename T> using SmallVector = std::vector<T>;
template<typename T> using SmallVectorImpl = std::vector<T>;
template<typename T> using SmallString = std::string;
template<typename T, typename U> using DenseMap = std::map<T, U>;
template<typename T> using MutableArrayRef = std::vector<T>&;
template<typename T> T* cast(void* ptr) { return static_cast<T*>(ptr); }
template<typename T> const T* cast(const void* ptr) { return static_cast<const T*>(ptr); }
template<typename T> T* dyn_cast(void* ptr) { return dynamic_cast<T*>(static_cast<T*>(ptr)); }
template<typename T> const T* dyn_cast(const void* ptr) { return dynamic_cast<const T*>(static_cast<const T*>(ptr)); }
template<typename T> bool isa(const void* ptr) { return dynamic_cast<const T*>(static_cast<const T*>(ptr)) != nullptr; }
}
#endif
