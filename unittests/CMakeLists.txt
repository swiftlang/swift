
include(AddSwiftUnittests)

if(SWIFT_INCLUDE_TOOLS)
  # We can't link C++ unit tests unless we build the tools.

  add_subdirectory(AST)
  add_subdirectory(Basic)
  add_subdirectory(Driver)
  add_subdirectory(IDE)
  add_subdirectory(Parse)
  add_subdirectory(SwiftDemangle)
  add_subdirectory(Syntax)

  if(SWIFT_BUILD_SDK_OVERLAY)
    # Runtime tests depend on symbols in StdlibUnittest.
    #
    # FIXME: cross-compile runtime unittests.
    add_subdirectory(runtime)
    add_subdirectory(Reflection)
  endif()

  if(SWIFT_BUILD_SOURCEKIT)
    add_subdirectory(SourceKit)
  endif()
endif()

