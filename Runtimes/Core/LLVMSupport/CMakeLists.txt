add_library(swiftLLVMSupport OBJECT
  ErrorHandling.cpp
  Hashing.cpp
  MemAlloc.cpp
  SmallPtrSet.cpp
  SmallVector.cpp
  StringRef.cpp)
target_compile_options(swiftLLVMSupport
  PRIVATE
    $<$<BOOL:${SwiftCore_HAS_ASL}>:-DSWIFT_STDLIB_HAS_ASL>)

if(NOT BUILD_SHARED_LIBS)
  install(TARGETS swiftLLVMSupport
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
endif()
