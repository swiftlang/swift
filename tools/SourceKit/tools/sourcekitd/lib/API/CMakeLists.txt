set(sourcekitdAPI_sources
  CodeCompletionResultsArray.cpp
  CompactArray.cpp
  DocSupportAnnotationArray.cpp
  Requests.cpp
  sourcekitdAPI-Common.cpp
  TokenAnnotationsArray.cpp
)

if(APPLE)
  list(APPEND sourcekitdAPI_sources
    sourcekitdAPI-XPC.cpp
  )
endif()

add_sourcekit_library(sourcekitdAPI
  ${sourcekitdAPI_sources}
  DEPENDS SourceKitSupport
)
