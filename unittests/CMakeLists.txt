
if(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
  if((NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "Clang") AND
     (NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "AppleClang"))
    message(FATAL_ERROR "Building the swift runtime is not supported with ${CMAKE_C_COMPILER_ID}. Use the just-built clang instead.")
  else()
    message(WARNING "Building the swift runtime using the host compiler, and not the just-built clang.")
  endif()
else()
  # If we use Clang-cl or MSVC, CMake provides default compiler and linker flags that are incompatible
  # with the frontend of Clang or Clang++.
  if(SWIFT_COMPILER_IS_MSVC_LIKE)
    set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang-cl")
    set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang-cl")
  else()
    set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang++")
    set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang")
  endif()
  
  set(CMAKE_CXX_COMPILER_ARG1 "")
  set(CMAKE_C_COMPILER_ARG1 "")
endif()

if(SWIFT_RUNTIME_USE_SANITIZERS)
  # TODO: Refactor this
  if("Thread" IN_LIST SWIFT_RUNTIME_USE_SANITIZERS)
    list(APPEND SWIFT_RUNTIME_CXX_FLAGS "-fsanitize=thread")
    list(APPEND SWIFT_RUNTIME_LINK_FLAGS "-fsanitize=thread")
    list(APPEND SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS "-sanitize=thread")
    list(APPEND SWIFT_RUNTIME_SWIFT_LINK_FLAGS "-fsanitize=thread")
  endif()
endif()

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

