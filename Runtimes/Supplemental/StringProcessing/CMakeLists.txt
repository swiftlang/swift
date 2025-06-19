cmake_minimum_required(VERSION 3.29)

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

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

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules")

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

find_package(SwiftCore REQUIRED)

include(GNUInstallDirs)

include(AvailabilityMacros)
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(ResourceEmbedding)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${PROJECT_NAME}_INSTALL_NESTED_SUBDIR>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${PROJECT_NAME}_INSTALL_NESTED_SUBDIR>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>")

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization"
  ${SwiftCore_ENABLE_PRESPECIALIZATION})

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")

add_subdirectory(_RegexParser)
add_subdirectory(_StringProcessing)
add_subdirectory(RegexBuilder)
