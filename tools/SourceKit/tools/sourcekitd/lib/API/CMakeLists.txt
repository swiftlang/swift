include(CheckIncludeFiles)

check_include_files("xpc/xpc.h" HAVE_XPC_H)

# If we were going to build for APPLE but don't have XPC, just build inproc.
if(APPLE AND NOT HAVE_XPC_H)
  set(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY TRUE)
endif()

set(sourcekitdAPI_sources
  CodeCompletionResultsArray.cpp
  CompactArray.cpp
  DocStructureArray.cpp
  DocSupportAnnotationArray.cpp
  Requests.cpp
  sourcekitdAPI-Common.cpp
  TokenAnnotationsArray.cpp
)

set(sourcekitdAPI_Darwin_sources
  sourcekitdAPI-XPC.cpp)
set(sourcekitdAPI_NonDarwin_InProc_sources
  sourcekitdAPI-InProc.cpp)
set(LLVM_OPTIONAL_SOURCES ${sourcekitdAPI_Darwin_sources} ${sourcekitdAPI_NonDarwin_InProc_sources})

if(APPLE AND HAVE_XPC_H)
  list(APPEND sourcekitdAPI_sources ${sourcekitdAPI_Darwin_sources})
elseif(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY)
  list(APPEND sourcekitdAPI_sources ${sourcekitdAPI_NonDarwin_InProc_sources})
endif()

add_sourcekit_library(sourcekitdAPI
  ${sourcekitdAPI_sources}
  DEPENDS
      SourceKitSupport SourceKitSwiftLang
)
