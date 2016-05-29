set(sourcekitdAPI_sources
  CodeCompletionResultsArray.cpp
  CompactArray.cpp
  DocSupportAnnotationArray.cpp
  Requests.cpp
  sourcekitdAPI-Common.cpp
  TokenAnnotationsArray.cpp
)

set(sourcekitdAPI_InProc_sources sourcekitdAPI-InProc.cpp)
set(sourcekitdAPI_XPC_sources sourcekitdAPI-XPC.cpp)

set(LLVM_OPTIONAL_SOURCES ${sourcekitdAPI_InProc_sources})
set(LLVM_OPTIONAL_SOURCES ${sourcekitdAPI_XPC_sources})

if(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  list(APPEND sourcekitdAPI_sources ${sourcekitdAPI_InProc_sources})
else()
  list(APPEND sourcekitdAPI_sources ${sourcekitdAPI_XPC_sources})
endif()

add_sourcekit_library(sourcekitdAPI
  ${sourcekitdAPI_sources}
  DEPENDS SourceKitSupport
)
