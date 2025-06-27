cmake_minimum_required(VERSION 3.29)

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules")

if($ENV{BUILD_NUMBER})
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftStringProcessing
  LANGUAGES Swift C
  VERSION 6.1.0${BUILD_NUMBER})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(FATAL_ERROR "Swift StringProcessing must build as a standalone project")
endif()

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

find_package(SwiftCore)

include(AvailabilityMacros)

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>")

add_subdirectory(_RegexParser)
add_subdirectory(_StringProcessing)
add_subdirectory(RegexBuilder)
