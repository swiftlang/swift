set(SourceKitSupport_sources
  FuzzyStringMatcher.cpp
  Logging.cpp
  ImmutableTextBuffer.cpp
  ThreadSafeRefCntPtr.cpp
  Tracing.cpp
  UIDRegistry.cpp
)

if(APPLE)
  list(APPEND SourceKitSupport_sources
    Concurrency-Mac.cpp
  )
endif()

add_sourcekit_library(SourceKitSupport
  ${SourceKitSupport_sources}
  DEPENDS swiftBasic clangBasic clangRewrite
)
