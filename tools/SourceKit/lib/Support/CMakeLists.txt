set(SourceKitSupport_sources
  Concurrency-libdispatch.cpp
  FuzzyStringMatcher.cpp
  Logging.cpp
  ImmutableTextBuffer.cpp
  ThreadSafeRefCntPtr.cpp
  Tracing.cpp
  UIDRegistry.cpp
)

set(SOURCEKIT_SUPPORT_DEPEND swiftBasic clangBasic clangRewrite)
if(SOURCEKIT_NEED_EXPLICIT_LIBDISPATCH)
  list(APPEND SOURCEKIT_SUPPORT_DEPEND dispatch BlocksRuntime)
endif()

add_sourcekit_library(SourceKitSupport
  ${SourceKitSupport_sources}
  DEPENDS ${SOURCEKIT_SUPPORT_DEPEND}
)
