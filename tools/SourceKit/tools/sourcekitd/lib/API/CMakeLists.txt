set(LLVM_OPTIONAL_SOURCES
  sourcekitdAPI-XPC.cpp
  sourcekitdAPI-InProc.cpp)

add_sourcekit_library(sourcekitdAPI
  CodeCompletionResultsArray.cpp
  CompactArray.cpp
  DocStructureArray.cpp
  DocSupportAnnotationArray.cpp
  RawData.cpp
  Requests.cpp
  sourcekitdAPI-Common.cpp
  TokenAnnotationsArray.cpp
  ExpressionTypeArray.cpp
)
target_link_libraries(sourcekitdAPI PRIVATE
  SourceKitSupport
  SourceKitSwiftLang)

if(APPLE AND HAVE_XPC_H)
  target_sources(sourcekitdAPI PRIVATE
    sourcekitdAPI-XPC.cpp)
elseif(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  target_sources(sourcekitdAPI PRIVATE
    sourcekitdAPI-InProc.cpp)
endif()
