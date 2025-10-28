add_library(swiftLLVMSupport OBJECT
  ErrorHandling.cpp
  Hashing.cpp
  MemAlloc.cpp
  SmallPtrSet.cpp
  SmallVector.cpp
  StringRef.cpp)
target_compile_options(swiftLLVMSupport PRIVATE
  $<$<BOOL:${SwiftCore_HAS_ASL}>:-DSWIFT_STDLIB_HAS_ASL>)
target_link_libraries(swiftLLVMSupport PRIVATE
  $<$<PLATFORM_ID:Android>:log>)

if(NOT BUILD_SHARED_LIBS)
  install(TARGETS swiftLLVMSupport
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
endif()
