set(SourceKitSupport_sources
  Concurrency-libdispatch.cpp
  FuzzyStringMatcher.cpp
  Logging.cpp
  ImmutableTextBuffer.cpp
  ThreadSafeRefCntPtr.cpp
  Tracing.cpp
  UIDRegistry.cpp
)

add_sourcekit_library(SourceKitSupport
  ${SourceKitSupport_sources}
  LINK_LIBS swiftBasic swiftSyntax clangBasic clangRewrite
)
if(SOURCEKIT_NEED_EXPLICIT_LIBDISPATCH)
  target_link_libraries(SourceKitSupport INTERFACE dispatch BlocksRuntime)
endif()

