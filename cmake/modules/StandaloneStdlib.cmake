
# CMAKE_SOURCE_DIR is the directory that cmake got initially invoked on.
# CMAKE_CURRENT_SOURCE_DIR is the current directory. If these are equal, it's
# a top-level build of the CMAKE_SOURCE_DIR. Otherwise, define a guard variable
# and return.
if(DEFINED SWIFT_MASTER_LOADED OR
   NOT ${CMAKE_CURRENT_SOURCE_DIR} STREQUAL ${CMAKE_SOURCE_DIR})
  set(SWIFT_MASTER_LOADED TRUE)
  return()
endif()

set(CMAKE_INSTALL_PREFIX "${SWIFT_DEST_ROOT}${TOOLCHAIN_DIR}/usr")

# Only happens if it's called from a top-level cmake invocation.
set(BUILD_STANDALONE TRUE)
# FIXME(compnerd) this shouldnt be set, but
# stdlib/public/SwiftShims/CMakeLists.txt depends on it for some reason
set(SWIFT_BUILT_STANDALONE TRUE)
set(SWIFT_BUILD_STDLIB TRUE)
if(BUILD_SHARED_LIBS)
  set(SWIFT_BUILD_DYNAMIC_STDLIB TRUE)
else()
  set(SWIFT_BUILD_DYNAMIC_STDLIB FALSE)
endif()
set(SWIFT_STDLIB_BUILD_TYPE ${CMAKE_BUILD_TYPE})

list(APPEND CMAKE_MODULE_PATH
     "${CMAKE_SOURCE_DIR}/../cmake/modules"
     "${SWIFT_PATH_TO_LLVM_SOURCE}/cmake/modules"
     "${SWIFT_PATH_TO_LLVM_BUILD}/lib/cmake/llvm")

include(AddSwift)
include(SwiftComponents)
include(SwiftConfigureSDK)
include(SwiftHandleGybSources)
include(SwiftSharedCMakeConfig)
include(SwiftSource)
include(SwiftUtils)

include(AddLLVM)
include(LLVMConfig)
include(HandleLLVMOptions)

# TODO(compnerd) this should be computed via the configure_sdk call based on
# CMAKE_SYSTEM_NAME, CMAKE_SYSTEM_PROCESSOR
set(SWIFT_HOST_VARIANT_SDK LINUX)
set(SWIFT_HOST_VARIANT_ARCH x86_64)
set(SWIFT_SDKS LINUX)
set(SWIFT_SDK_LINUX_ARCHITECTURES x86_64)
set(SWIFT_SDK_LINUX_LIB_SUBDIR linux)
set(SWIFT_SDK_LINUX_PATH "/")
set(SWIFT_SDK_LINUX_ARCH_x86_64_TRIPLE x86_64-pc-linux-gnu)
set(SWIFT_LINUX_ICU_UC "/usr/x86_64-unknown-linux-gnu/lib/libicuuc.so")
set(SWIFT_LINUX_ICU_I18N "/usr/x86_64-unknown-linux-gnu/lib/libicui18n.so")

