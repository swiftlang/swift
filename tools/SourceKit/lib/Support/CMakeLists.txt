set(SourceKitSupport_sources
  FuzzyStringMatcher.cpp
  Logging.cpp
  ImmutableTextBuffer.cpp
  ThreadSafeRefCntPtr.cpp
  Tracing.cpp
  UIDRegistry.cpp
)

set(SourceKitSupport_Darwin_sources
  Concurrency-Mac.cpp)
set(LLVM_OPTIONAL_SOURCES ${SourceKitSupport_Darwin_sources})
if(APPLE)
  list(APPEND SourceKitSupport_sources ${SourceKitSupport_Darwin_sources})
endif()

add_sourcekit_library(SourceKitSupport
  ${SourceKitSupport_sources}
  DEPENDS swiftBasic clangBasic clangRewrite
)
