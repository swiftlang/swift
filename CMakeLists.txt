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
  if( NOT EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/llvm-tblgen${CMAKE_EXECUTABLE_SUFFIX}" )
    if ( NOT EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/Debug/llvm-tblgen${CMAKE_EXECUTABLE_SUFFIX}" )
      message(FATAL_ERROR "Please set SWIFT_PATH_TO_LLVM_BUILD to a directory containing an LLVM build.")
    endif()
  endif()

  set(SWIFT_PATH_TO_CLANG_SOURCE "${SWIFT_PATH_TO_LLVM_SOURCE}/tools/clang"
      CACHE PATH "Path to Clang source code.")
  set(SWIFT_PATH_TO_CLANG_BUILD "" CACHE PATH
    "Path to the directory where Clang was built or installed.")

  if( NOT EXISTS "${SWIFT_PATH_TO_CLANG_SOURCE}/include/clang/AST/Decl.h" )
    message(FATAL_ERROR "Please set SWIFT_PATH_TO_CLANG_SOURCE to the root directory of Clang's source code.")
  else()
    get_filename_component(CLANG_MAIN_SRC_DIR ${SWIFT_PATH_TO_CLANG_SOURCE}
      ABSOLUTE)
  endif()

  if(EXISTS "${SWIFT_PATH_TO_CLANG_BUILD}/include/clang/Basic/Version.inc")
    set(CLANG_BUILD_INCLUDE_DIR "${SWIFT_PATH_TO_CLANG_BUILD}/include")
  elseif(EXISTS "${SWIFT_PATH_TO_CLANG_BUILD}/tools/clang/include/clang/Basic/Version.inc")
    set(CLANG_BUILD_INCLUDE_DIR "${SWIFT_PATH_TO_CLANG_BUILD}/tools/clang/include")
  else()
    message(FATAL_ERROR "Please set SWIFT_PATH_TO_CLANG_BUILD to a directory containing a Clang build.")
  endif()
  if (CLANG_MAIN_INCLUDE_DIR)
    get_filename_component(CLANG_MAIN_SRC_DIR ${SWIFT_PATH_TO_CLANG_SOURCE}
    ABSOLUTE)
  endif()
  message("${CLANG_MAIN_INCLUDE_DIR}")

  list(APPEND CMAKE_MODULE_PATH "${SWIFT_PATH_TO_LLVM_BUILD}/share/llvm/cmake")

  get_filename_component(PATH_TO_LLVM_BUILD ${SWIFT_PATH_TO_LLVM_BUILD}
    ABSOLUTE)
  get_filename_component(PATH_TO_CLANG_BUILD ${SWIFT_PATH_TO_CLANG_BUILD}
    ABSOLUTE)

  include(AddLLVM)
  include(LLVMParseArguments)
  include(TableGen)
  include("${SWIFT_PATH_TO_LLVM_BUILD}/share/llvm/cmake/LLVMConfig.cmake")
  include(HandleLLVMOptions)

  set(PACKAGE_VERSION "${LLVM_PACKAGE_VERSION}")

  set(LLVM_MAIN_INCLUDE_DIR "${LLVM_MAIN_SRC_DIR}/include")
  set(CLANG_MAIN_INCLUDE_DIR "${CLANG_MAIN_SRC_DIR}/include")
  set(LLVM_BINARY_DIR ${CMAKE_BINARY_DIR})

  set(CMAKE_INCLUDE_CURRENT_DIR ON)
  include_directories("${PATH_TO_LLVM_BUILD}/include" 
                      "${LLVM_MAIN_INCLUDE_DIR}"
                      "${CLANG_BUILD_INCLUDE_DIR}"
                      "${CLANG_MAIN_INCLUDE_DIR}")
  link_directories("${PATH_TO_LLVM_BUILD}/lib"
                   "${PATH_TO_CLANG_BUILD}/lib")

  if( EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/tblgen${CMAKE_EXECUTABLE_SUFFIX}" )
    set(LLVM_TABLEGEN_EXE "${PATH_TO_LLVM_BUILD}/bin/tblgen")
  else()
    # FIXME: The fallback to a 'Debug' tblgen is madness
    set(LLVM_TABLEGEN_EXE "${PATH_TO_LLVM_BUILD}/bin/Debug/tblgen")
  endif()
  
  set(LIT_ARGS_DEFAULT "-sv")
  if (MSVC OR XCODE)
    set(LIT_ARGS_DEFAULT "${LIT_ARGS_DEFAULT} --no-progress-bar")
  endif()
  set(LLVM_LIT_ARGS "${LIT_ARGS_DEFAULT}" CACHE STRING "Default options for lit")

  set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
  set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
  set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )

  set( SWIFT_BUILT_STANDALONE 1 )
endif()

set(SWIFT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SWIFT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

option(SWIFT_OPTIMIZED "Enable optimization for Swift source files" YES)
option(SWIFT_ASSERTS "Enable assertions for the Swift standard library" YES)
set(SWIFT_MODULE_CACHE_PATH "" CACHE PATH
  "Directory to use as the Clang module cache when building Swift source files")

# Xcode: use libc++ and c++11 using proper build settings.
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
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNINITIALIZED_AUTOS "YES")

  # Disable RTTI
  set(CMAKE_XCODE_ATTRIBUTE_GCC_ENABLE_CPP_RTTI "NO")

  # Disable exceptions
  set(CMAKE_XCODE_ATTRIBUTE_GCC_ENABLE_CPP_EXCEPTIONS "NO") 
else ()
  # Compile with C++11, without RTTI, exceptions
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -stdlib=libc++ -fno-rtti -fno-exceptions")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stdlib=libc++")
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -stdlib=libc++")
endif ()

# Compile a swift file into an object file (as a library).
#
# Usage:
#   compile_swift_files(OUTPUT objfile                      # Name of the resulting object file
#     SOURCES swift_src [swift_src swift_src...]  # Swift source files to compile
#     [IS_MAIN])                                  # This file is not a library but an executable
function(compile_swift_files)	
  parse_arguments(SWIFTFILE "OUTPUT;SOURCES" "IS_MAIN" ${ARGN})
  
  set(swift_flags)

  set(source_files)
  foreach(file ${SWIFTFILE_SOURCES})
    # Determine where this file is.
    get_filename_component(file_path ${file} PATH) 
    if(IS_ABSOLUTE "${file_path}")
      list(APPEND source_files "${file}")
    else()
      list(APPEND source_files "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
    endif()
  endforeach()

  # Compute flags to be passed down to swift when compiling a swift
  # file.
  if(SWIFT_OPTIMIZED)
    list(APPEND swift_flags "-O2")
  else()
    list(APPEND swift_flags "-O0")
  endif()
  
  # If we have a modules SDK, use it for building.
  if (HAVE_MODULES_SDK)
    list(APPEND swift_flags "-sdk=${MODULES_SDK}")
  endif()

  # If we have a custom module cache path, use it.
  if (SWIFT_MODULE_CACHE_PATH)
    list(APPEND swift_flags "-module-cache-path=${SWIFT_MODULE_CACHE_PATH}")
  endif()

  # FIXME: Cleaner way to do this?

  if (NOT SWIFTFILE_IS_MAIN)
    list(APPEND swift_flags -parse-as-library)
  endif()

  # Use the constraint checker, because we can
  list(APPEND swift_flags "-constraint-checker")

  set(objfile ${SWIFTFILE_OUTPUT})
  if (NOT IS_ABSOLUTE ${objfile})
    set(objfile
      "${CMAKE_CURRENT_BINARY_DIR}/${objfile}")
  endif()

  if(SWIFTFILE_IS_MAIN)
    set(module_source_file)
  else()
    # Compute the module source file; a hack until we get real module
    # compilation.  In principle this separate file might not be needed
    # except for swift.swift, which is composed of multiple .swift
    # files.
    get_filename_component(module_source_name ${objfile} NAME_WE)
    set(module_source_dir ${SWIFTLIB_DIR}/swift)
    set(module_source_file ${module_source_dir}/${module_source_name}.swift)

    add_custom_command(
      OUTPUT ${module_source_file}
      COMMAND ${CMAKE_COMMAND} -E make_directory ${module_source_dir}
      COMMAND cat ${source_files} > "${module_source_file}"
      DEPENDS ${source_files}
      COMMENT "FIXME: generating module source file ${module_source_name}.swift")
    set_source_files_properties("${module_source_file}" PROPERTIES GENERATED ON)
  endif()

  add_custom_command(
    OUTPUT ${objfile}
    COMMAND swift "-c" ${swift_flags} "-o" "${objfile}" ${source_files}
    DEPENDS swift ${source_files} ${module_source_file}
#    MAIN_DEPENDENCY ${target}
    COMMENT "Compiling ${objfile}")
  
  # Make sure the build system knows the file is a generated object file.
  set_source_files_properties(${objfile}
    PROPERTIES 
    GENERATED true 
    EXTERNAL_OBJECT true
    LANGUAGE C
    OBJECT_DEPENDS "${source_files}")

  # Make sure this object file gets cleaned up.
  set_directory_properties(
    PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${objfile}")
endfunction()

# Compile an assembler file into an object file.
#
# Usage:
#   compile_assembler_file(file     # Name of the source file, e.g., swift.swift)
#     target                        # The target of which this file is a part
#     objectvar)                    # Variable to receive the object file name
function(compile_assembler_file file target objectvar)
  # Determine where this file is.
  get_filename_component(file_path ${file} PATH) 
  if(IS_ABSOLUTE "${file_path}")
    set(source_file "${file}")
  else()
    set(source_file "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
  endif()

  get_filename_component(file_name ${file} NAME_WE)
  set(objfile
    "${CMAKE_CURRENT_BINARY_DIR}/${file_name}${CMAKE_C_OUTPUT_EXTENSION}")
  add_custom_command(
    OUTPUT ${objfile}
    COMMAND "clang" "-c" "-o" "${objfile}" "${source_file}" 
            "-I${SWIFT_SOURCE_DIR}/include"
    DEPENDS ${source_file}
#    MAIN_DEPENDENCY ${target}
    COMMENT "Compiling ${file_name}${CMAKE_C_OUTPUT_EXTENSION}")
  
  # Make sure the build system knows the file is a generated object file.
  set_source_files_properties(${objfile}
    PROPERTIES 
    GENERATED true 
    EXTERNAL_OBJECT true
    OBJECT_DEPENDS "${source_file}")

  # Make sure this object file gets cleaned up.
  set_directory_properties(
    PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${objfile}")

  # Pass the object file name back to the caller.
  set(${objectvar} "${objfile}" PARENT_SCOPE)
endfunction()

# Process the sources within the given variable, pulling out any Swift
# sources to be compiled with 'swift' directly. This updates
# ${sourcesvar} in place with the resulting list and ${externalvar} with the
# list of externally-build sources.
#
# Usage:
#   handle_swift_sources(sourcesvar externalvar target)
function(handle_swift_sources sourcesvar externalvar target)
  parse_arguments(SWIFTSOURCES  "" "IS_MAIN" ${ARGN})

  set(result)
  set(swift_sources)
  set(asm_sources)
  foreach(src ${${sourcesvar}})
    get_filename_component(extension ${src} EXT)
    if(extension STREQUAL ".swift")
      list(APPEND swift_sources ${src})
    elseif(extension STREQUAL ".s")
      # We have an assembler file; compile it.
      compile_assembler_file(${src} ${target} "obj")
      list(APPEND result ${obj})
      list(APPEND asm_sources ${obj})
    else()
      list(APPEND result ${src})
    endif()
  endforeach()

  set(IS_MAIN_arg)
  if (SWIFTSOURCES_IS_MAIN)
    set(IS_MAIN_arg IS_MAIN)
  endif()

  if(swift_sources)

    # Special-case hack to create swift.o for the swift_stdlib target
    if(target STREQUAL swift_stdlib)
      set(swift_obj_base "swift")
    else()
      # Otherwise, get the name from the first swift input file.  Also
      # a hack!
      list(GET swift_sources 0 swift_obj_base)
      get_filename_component(swift_obj_base ${swift_obj_base} NAME_WE)
    endif()

    set(swift_obj "${swift_obj_base}${CMAKE_C_OUTPUT_EXTENSION}")
    compile_swift_files(OUTPUT ${swift_obj} SOURCES ${swift_sources} ${IS_MAIN_arg})
    list(APPEND result ${swift_obj})
  endif()

  llvm_process_sources(result ${result})
  set(${sourcesvar} "${result}" PARENT_SCOPE)
  set(${externalvar} ${swift_sources} ${asm_sources} PARENT_SCOPE)
endfunction()

# Set SWIFTLIB_DIR to the directory in the build tree where Swift source files
# should be placed. Note that $CMAKE_CFG_INTDIR expands to "." for
# single-configuration builds.
set(SWIFTLIB_DIR "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}")

# Add a new Swift library.
#
# Usage:
#   add_swift_library(name           # Name of the library (e.g., swiftParse)
#     [DEPENDS dep1 ...]             # Libraries this library depends on
#     [COMPONENT_DEPENDS comp1 ...]  # LLVM components this library depends on
#     [INSTALL]                      # Install library to lib/swift
#     source1 [source2 source3 ...]) # Sources to add into this library
macro(add_swift_library name)
  parse_arguments(SWIFTLIB  "DEPENDS;COMPONENT_DEPENDS" "INSTALL" ${ARGN})
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

  handle_swift_sources(SWIFTLIB_SOURCES SWIFTLIB_EXTERNAL_SOURCES ${name})
  add_library( ${name} ${libkind} ${SWIFTLIB_SOURCES} )

  if( LLVM_COMMON_DEPENDS )
    add_dependencies( ${name} ${LLVM_COMMON_DEPENDS} )
  endif( LLVM_COMMON_DEPENDS )

  # FIXME: This is a hack to work around CMake's apparent inability to tell Xcode
  # to rebuild the exe if any of the object files changed.
  if (SWIFTLIB_EXTERNAL_SOURCES)
    add_custom_target("${name}-external" DEPENDS ${SWIFTLIB_EXTERNAL_SOURCES})
    add_dependencies(${name} "${name}-external")
  endif ()

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

  if(SWIFTLIB_INSTALL)
    set_target_properties(${name} PROPERTIES
      INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib/swift")

    # Install runtime libraries to lib/swift instead of lib. This works around
    # the fact that -isysroot prevents linking to libraries in the system
    # /usr/lib if Swift is installed in /usr.
    set_target_properties(${name} PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/swift
      ARCHIVE_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/swift)

    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER ${config} config_upper)
      # Hack to deal with the fact that ${SWIFTLIB_DIR} contains the build-time
      # variable $(CONFIGURATION). Note that this fix is Xcode-specific.
      # You might think you could just replace ${CMAKE_CFG_INTDIR} here, but
      # that can actually have other things mangled into it besides just the
      # configuration. This at least doesn't pretend to do the right thing
      # outside of Xcode.
      string(REPLACE "$(CONFIGURATION)" ${config} config_lib_dir
        ${SWIFTLIB_DIR})
      set_target_properties(${name} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/swift
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/swift)
    endforeach()

    install(TARGETS ${name}
      LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift
      ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift)

    # FIXME: Install .swift files so they can be parsed on import.
    # Really we want to install a preparsed metadata file.
    foreach(external_source ${SWIFTLIB_EXTERNAL_SOURCES})
      get_filename_component(extension ${external_source} EXT)
      if(extension STREQUAL ".swift")
        set(filename ${external_source})
        install(FILES ${filename}
          DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift)
      endif()
    endforeach(external_source)
  endif(SWIFTLIB_INSTALL)

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
  set(SWIFTEXE)
  
  handle_swift_sources(SWIFTEXE_DEFAULT_ARGS SWIFTEXE_EXTERNAL_SOURCES ${name} IS_MAIN)

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

  # FIXME: This is a hack to work around CMake's apparent inability to tell Xcode
  # to rebuild the exe if any of the object files changed.
  if (SWIFTEXE_EXTERNAL_SOURCES)
    add_custom_target("${name}-external" DEPENDS ${SWIFTEXE_EXTERNAL_SOURCES})
    add_dependencies(${name} "${name}-external")
  endif ()

  target_link_libraries( ${name} ${SWIFTEXE_DEPENDS} )
  llvm_config( ${name} ${SWIFTEXE_COMPONENT_DEPENDS} )
  target_link_libraries( ${name} ${LLVM_COMMON_LIBS} )
  link_system_libs( ${name} )

  set_target_properties(${name} PROPERTIES FOLDER "Swift executables")  
endmacro(add_swift_executable)

set(MODULES_SDK "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk" CACHE PATH
  "Path to the modules-enabled SDK to use")
if(EXISTS "${MODULES_SDK}/System/Library/Frameworks/module.map")
  set(HAVE_MODULES_SDK YES)
else()
  message(STATUS "warning: modules-enabled SDK is not available")
endif()

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
add_subdirectory(stdlib)
add_subdirectory(utils)

if(HAVE_MODULES_SDK)
  add_subdirectory(objc)
endif()

add_subdirectory(examples)
add_subdirectory(unittests)
add_subdirectory(docs)

# Add a documentation target so that documentation shows up in the
# Xcode project.
add_custom_target(Documentation
  SOURCES README.txt
  docs/LangRef.html
  docs/toc.js)

# Configure CPack.
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_RELOCATABLE "false")
set(CPACK_PACKAGE_VENDOR "LLVM Project")
set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY "OFF")
set(CPACK_SET_DESTDIR "ON")

set(CPACK_PACKAGE_NAME "swift")
set(CPACK_SYSTEM_NAME "macosx")

# FIXME: Real version number.
execute_process(COMMAND date "+%Y%m%d"
  OUTPUT_VARIABLE CPACK_PACKAGE_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE)

# CPack must be included *after* its configuration variables are set.
include(CPack)
