cmake_minimum_required(VERSION 3.26...3.29)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")
project(SwiftCore LANGUAGES C CXX Swift VERSION 6.1)

# The Swift standard library is not intended for use as a sub-library as part of
# another project. It is tightly coupled with the compiler version.
if(NOT PROJECT_IS_TOP_LEVEL)
  message(FATAL_ERROR "Swift Core must build as a standalone project")
endif()
include(CompilerSettings)

include_directories(include)

add_subdirectory(LLVMSupport)
