# Support building Swift as a standalone project, using LLVM as an
# external library.
if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
  project(Swift)
  cmake_minimum_required(VERSION 2.8)

  set(SWIFT_PATH_TO_LLVM_SOURCE "" CACHE PATH
    "Path to LLVM source code. Not necessary if using an installed LLVM.")
  set(SWIFT_PATH_TO_LLVM_BUILD "" CACHE PATH
    "Path to the directory where LLVM was built or installed.")

  if( SWIFT_PATH_TO_LLVM_SOURCE )
    if( NOT EXISTS "${SWIFT_PATH_TO_LLVM_SOURCE}/cmake/config-ix.cmake" )
      message(FATAL_ERROR "Please set SWIFT_PATH_TO_LLVM_SOURCE to the root directory of LLVM source code.")
    else()
      get_filename_component(LLVM_MAIN_SRC_DIR ${SWIFT_PATH_TO_LLVM_SOURCE}
	ABSOLUTE)
      list(APPEND CMAKE_MODULE_PATH "${LLVM_MAIN_SRC_DIR}/cmake/modules")
    endif()
  endif()

  # FIXME: The check for a 'Debug' tblgen is madness
  if( NOT EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/tblgen${CMAKE_EXECUTABLE_SUFFIX}" )
    if ( NOT EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/Debug/tblgen${CMAKE_EXECUTABLE_SUFFIX}" )
      message(FATAL_ERROR "Please set SWIFT_PATH_TO_LLVM_BUILD to a directory containing an LLVM build.")
    endif()
  endif()

  list(APPEND CMAKE_MODULE_PATH "${SWIFT_PATH_TO_LLVM_BUILD}/share/llvm/cmake")

  get_filename_component(PATH_TO_LLVM_BUILD ${SWIFT_PATH_TO_LLVM_BUILD}
    ABSOLUTE)

  include(AddLLVM)
  include(LLVMParseArguments)
  include(TableGen)
  include("${SWIFT_PATH_TO_LLVM_BUILD}/share/llvm/cmake/LLVMConfig.cmake")
  include(HandleLLVMOptions)

  set(PACKAGE_VERSION "${LLVM_PACKAGE_VERSION}")

  set(LLVM_MAIN_INCLUDE_DIR "${LLVM_MAIN_SRC_DIR}/include")
  set(LLVM_BINARY_DIR ${CMAKE_BINARY_DIR})
  set(LLVM_TOOLS_BINARY_DIR ${CMAKE_BINARY_DIR})

  set(CMAKE_INCLUDE_CURRENT_DIR ON)
  include_directories("${PATH_TO_LLVM_BUILD}/include" "${LLVM_MAIN_INCLUDE_DIR}")
  link_directories("${PATH_TO_LLVM_BUILD}/lib")

  if( EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/tblgen${CMAKE_EXECUTABLE_SUFFIX}" )
    set(LLVM_TABLEGEN_EXE "${PATH_TO_LLVM_BUILD}/bin/tblgen")
  else()
    # FIXME: The fallback to a 'Debug' tblgen is madness
    set(LLVM_TABLEGEN_EXE "${PATH_TO_LLVM_BUILD}/bin/Debug/tblgen")
  endif()

  set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
  set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
  set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )

  set( SWIFT_BUILT_STANDALONE 1 )
endif()

set(SWIFT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SWIFT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

# Xcode: use libc++ and c++'0x using proper build settings.
if ( XCODE )
  # Force usage of Clang.
  set(CMAKE_XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0"
      CACHE STRING "Xcode Compiler")
  # Use C++'0X.
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LANGUAGE_STANDARD "c++0x"
      CACHE STRING "Xcode C++ Language Standard")
  # Use libc++.
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LIBRARY "libc++"
      CACHE STRING "Xcode C++ Standard Library")
  # Enable some warnings not enabled by default.
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_ABOUT_RETURN_TYPE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_VALUE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_VARIABLE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_SIGN_COMPARE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_FUNCTION "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_INITIALIZER_NOT_FULLY_BRACKETED "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_HIDDEN_VIRTUAL_FUNCTIONS "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_NON_VIRTUAL_DESTRUCTOR "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNINITIALIZED_AUTOS "YES")
endif ( XCODE )

# Add a new Swift library.
#
# Usage:
#   add_swift_library(name           # Name of the library (e.g., swiftParse)
#     [DEPENDS dep1 ...]             # Libraries this library depends on
#     [COMPONENT_DEPENDS comp1 ...]  # LLVM components this library depends on
#     source1 [source2 source3 ...]) # Sources to add into this library
macro(add_swift_library name)
  parse_arguments(SWIFTLIB  "DEPENDS;COMPONENT_DEPENDS" "" ${ARGN})
  set(SWIFTLIB_SOURCES ${SWIFTLIB_DEFAULT_ARGS})

  if(MSVC_IDE OR XCODE)
    string( REGEX MATCHALL "/[^/]+" split_path ${CMAKE_CURRENT_SOURCE_DIR})
    list( GET split_path -1 dir)
    file( GLOB_RECURSE SWIFTLIB_HEADERS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.h
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.td
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.def)
    set(SWIFTLIB_SOURCES ${SWIFTLIB_SOURCES} ${SWIFTLIB_HEADERS})
  endif(MSVC_IDE OR XCODE)

  if (MODULE)
    set(libkind MODULE)
  elseif (SHARED_LIBRARY)
    set(libkind SHARED)
  else()
    set(libkind)
  endif()

  add_library( ${name} ${libkind} ${SWIFTLIB_SOURCES} )

  if( LLVM_COMMON_DEPENDS )
    add_dependencies( ${name} ${LLVM_COMMON_DEPENDS} )
  endif( LLVM_COMMON_DEPENDS )

  target_link_libraries( ${name} ${SWIFTLIB_DEPENDS} )
  llvm_config( ${name} ${SWIFTLIB_COMPONENT_DEPENDS} )
  target_link_libraries( ${name} ${LLVM_COMMON_LIBS} )
  link_system_libs( ${name} )

  if(MSVC)
    get_target_property(cflag ${name} COMPILE_FLAGS)
    if(NOT cflag)
      set(cflag "")
    endif(NOT cflag)
    set(cflag "${cflag} /Za")
    set_target_properties(${name} PROPERTIES COMPILE_FLAGS ${cflag})
  endif(MSVC)

  install(TARGETS ${name}
    LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}
    ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX})

  set_target_properties(${name} PROPERTIES FOLDER "Swift libraries")
endmacro(add_swift_library)

# Add a new Swift executable.
#
# Usage:
#   add_swift_executable(name         # Name of the executable (e.g., swift)
#     [DEPENDS dep1 ...]              # Libraries this executable depends on
#     [COMPONENT_DEPENDS comp1 ...]   # LLVM components this executable 
#                                     # depends on
#     [EXCLUDE_FROM_ALL]              # Whether to exclude this executable from
#                                     # the ALL_BUILD target
#     source1 [source2 source3 ...])  # Sources to add into this library
macro(add_swift_executable name)
  # Parse the arguments we were given.
  parse_arguments(SWIFTEXE  "DEPENDS;COMPONENT_DEPENDS" "EXCLUDE_FROM_ALL" 
    ${ARGN})
  
  # Add the executable
  if (${SWIFTEXE_EXCLUDE_FROM_ALL})
    add_executable(${name} EXCLUDE_FROM_ALL ${SWIFTEXE_DEFAULT_ARGS})
  else()
    add_executable(${name} ${SWIFTEXE_DEFAULT_ARGS})
  endif()

  # Add appropriate dependencies
  if( LLVM_COMMON_DEPENDS )
    add_dependencies( ${name} ${LLVM_COMMON_DEPENDS} )
  endif( LLVM_COMMON_DEPENDS )

  target_link_libraries( ${name} ${SWIFTEXE_DEPENDS} )
  llvm_config( ${name} ${SWIFTEXE_COMPONENT_DEPENDS} )
  target_link_libraries( ${name} ${LLVM_COMMON_LIBS} )
  link_system_libs( ${name} )

  set_target_properties(${name} PROPERTIES FOLDER "Swift executables")  
endmacro(add_swift_executable)

# We'll need this once we have generated headers
include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  )

# Add all of the subdirectories, where we actually do work.
add_subdirectory(lib)
add_subdirectory(tools)
add_subdirectory(test)
add_subdirectory(runtime)

# Add a documentation target so that documentation shows up in the
# Xcode project.
add_custom_target(Documentation
  SOURCES README.txt
  docs/LangRef.html
  docs/swift.css
  docs/toc.js)
