
set(LLVM_DEFAULT_TARGET_TRIPLE)
set(LLVM_ENABLE_DUMP)
set(LLVM_ENABLE_THREADS YES)
set(LLVM_FORCE_ENABLE_STATS NO)
set(LLVM_HAS_ATOMICS YES)
set(LLVM_HOST_TRIPLE)
set(LLVM_NATIVE_ARCH)
if(SWIFT_PRIMARY_VARIANT_SDK STREQUAL Windows)
  set(LLVM_ON_UNIX NO)
else()
  set(LLVM_ON_UNIX YES)
endif()
set(LLVM_USE_INTEL_JITEVENTS NO)
set(LLVM_USE_OPROFILE NO)
set(LLVM_USE_PERF NO)

configure_file(${PROJECT_SOURCE_DIR}/include/llvm/Config/llvm-config.h.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/llvm/Config/llvm-config.h)

add_swift_target_library(swiftLLVMSupport OBJECT_LIBRARY
    ErrorHandling.cpp
    Hashing.cpp
    MemAlloc.cpp
    SmallPtrSet.cpp
    SmallVector.cpp
    StringRef.cpp
  INSTALL_IN_COMPONENT
    never_install)
