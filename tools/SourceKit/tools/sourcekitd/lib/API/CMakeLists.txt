set(sourcekitdAPI_sources
  CodeCompletionResultsArray.cpp
  CompactArray.cpp
  DocSupportAnnotationArray.cpp
  Requests.cpp
  sourcekitdAPI-Common.cpp
  TokenAnnotationsArray.cpp
)

set(sourcekitdAPI_Darwin_sources
  sourcekitdAPI-XPC.cpp)
set(LLVM_OPTIONAL_SOURCES ${sourcekitdAPI_Darwin_sources})
if(APPLE)
  list(APPEND sourcekitdAPI_sources ${sourcekitdAPI_Darwin_sources})
endif()

add_sourcekit_library(sourcekitdAPI
  ${sourcekitdAPI_sources}
  DEPENDS SourceKitSupport
)
