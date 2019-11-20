add_sourcekit_library(SourceKitSupport
  Concurrency-libdispatch.cpp
  FuzzyStringMatcher.cpp
  Logging.cpp
  ImmutableTextBuffer.cpp
  ThreadSafeRefCntPtr.cpp
  Tracing.cpp
  UIDRegistry.cpp)
target_link_libraries(SourceKitSupport PRIVATE
  swiftBasic
  swiftSyntax
  clangBasic
  clangRewrite)
if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
  target_link_libraries(SourceKitSupport INTERFACE
    dispatch
    BlocksRuntime)
endif()

