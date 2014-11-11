cmake_minimum_required(VERSION 2.8)

include(CMakeParseArguments)

# lipo is used to create universal binaries.
find_program(LIPO "lipo")

# Support building Swift as a standalone project, using LLVM as an
# external library.
if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
  project(Swift)

  # Rely on llvm-config.
  set(CONFIG_OUTPUT)
  find_program(LLVM_CONFIG "llvm-config")
  if(LLVM_CONFIG)
    message(STATUS "Found LLVM_CONFIG as ${LLVM_CONFIG}")
    set(CONFIG_COMMAND ${LLVM_CONFIG}
      "--assertion-mode"
      "--bindir"
      "--libdir"
      "--includedir"
      "--prefix"
      "--src-root")
    execute_process(
      COMMAND ${CONFIG_COMMAND}
      RESULT_VARIABLE HAD_ERROR
      OUTPUT_VARIABLE CONFIG_OUTPUT
    )
    if(NOT HAD_ERROR)
      string(REGEX REPLACE
        "[ \t]*[\r\n]+[ \t]*" ";"
        CONFIG_OUTPUT ${CONFIG_OUTPUT})
    else()
      string(REPLACE ";" " " CONFIG_COMMAND_STR "${CONFIG_COMMAND}")
      message(STATUS "${CONFIG_COMMAND_STR}")
      message(FATAL_ERROR "llvm-config failed with status ${HAD_ERROR}")
    endif()
  else()
    message(FATAL_ERROR "llvm-config not found -- ${LLVM_CONFIG}")
  endif()

  list(GET CONFIG_OUTPUT 0 ENABLE_ASSERTIONS)
  list(GET CONFIG_OUTPUT 1 TOOLS_BINARY_DIR)
  list(GET CONFIG_OUTPUT 2 LIBRARY_DIR)
  list(GET CONFIG_OUTPUT 3 INCLUDE_DIR)
  list(GET CONFIG_OUTPUT 4 LLVM_OBJ_ROOT)
  list(GET CONFIG_OUTPUT 5 MAIN_SRC_DIR)

  if(NOT MSVC_IDE)
    set(LLVM_ENABLE_ASSERTIONS ${ENABLE_ASSERTIONS}
      CACHE BOOL "Enable assertions")
    # Assertions should follow llvm-config's.
    mark_as_advanced(LLVM_ENABLE_ASSERTIONS)
  endif()

  set(LLVM_TOOLS_BINARY_DIR ${TOOLS_BINARY_DIR} CACHE PATH "Path to llvm/bin")
  set(LLVM_LIBRARY_DIR ${LIBRARY_DIR} CACHE PATH "Path to llvm/lib")
  set(LLVM_MAIN_INCLUDE_DIR ${INCLUDE_DIR} CACHE PATH "Path to llvm/include")
  set(LLVM_BINARY_DIR ${LLVM_OBJ_ROOT} CACHE PATH "Path to LLVM build tree")
  set(LLVM_MAIN_SRC_DIR ${MAIN_SRC_DIR} CACHE PATH "Path to LLVM source tree")

  find_program(LLVM_TABLEGEN_EXE "llvm-tblgen" ${LLVM_TOOLS_BINARY_DIR}
    NO_DEFAULT_PATH)

  set(LLVM_CMAKE_PATH "${LLVM_BINARY_DIR}/share/llvm/cmake")
  set(LLVMCONFIG_FILE "${LLVM_CMAKE_PATH}/LLVMConfig.cmake")
  if(EXISTS ${LLVMCONFIG_FILE})
    list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_PATH}")
    include(${LLVMCONFIG_FILE})
  else()
    message(FATAL_ERROR "Not found: ${LLVMCONFIG_FILE}")
  endif()

  # Clang
  set(SWIFT_PATH_TO_LLVM_SOURCE "${LLVM_MAIN_SRC_DIR}")
  set(SWIFT_PATH_TO_LLVM_BUILD "${LLVM_BINARY_DIR}")

  set(PATH_TO_LLVM_SOURCE "${SWIFT_PATH_TO_LLVM_SOURCE}")
  set(PATH_TO_LLVM_BUILD "${SWIFT_PATH_TO_LLVM_BUILD}")

  set(SWIFT_PATH_TO_CLANG_SOURCE "${SWIFT_PATH_TO_LLVM_SOURCE}/tools/clang"
      CACHE PATH "Path to Clang source code.")
  set(SWIFT_PATH_TO_CLANG_BUILD "${SWIFT_PATH_TO_LLVM_BUILD}" CACHE PATH
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

  list(APPEND CMAKE_MODULE_PATH "${SWIFT_PATH_TO_LLVM_BUILD}/share/llvm/cmake")

  get_filename_component(PATH_TO_LLVM_BUILD ${SWIFT_PATH_TO_LLVM_BUILD}
    ABSOLUTE)
  get_filename_component(PATH_TO_CLANG_BUILD ${SWIFT_PATH_TO_CLANG_BUILD}
    ABSOLUTE)

  # MSVC has a gazillion warnings with this.
  if( MSVC )
    set(LLVM_ENABLE_WARNINGS "Enable compiler warnings." OFF)
  else( MSVC )
    set(LLVM_ENABLE_WARNINGS "Enable compiler warnings." ON)
  endif()

  include(AddLLVM)
  include(TableGen)
  include(HandleLLVMOptions)
  include(LLVMParseArguments)

  set(PACKAGE_VERSION "${LLVM_PACKAGE_VERSION}")

  set(LLVM_MAIN_INCLUDE_DIR "${LLVM_MAIN_SRC_DIR}/include")
  set(CLANG_MAIN_INCLUDE_DIR "${CLANG_MAIN_SRC_DIR}/include")
  set(LLVM_BINARY_DIR ${CMAKE_BINARY_DIR})

  set(CMAKE_INCLUDE_CURRENT_DIR ON)
  include_directories("${PATH_TO_LLVM_BUILD}/include" 
                      "${LLVM_MAIN_INCLUDE_DIR}"
                      "${CLANG_BUILD_INCLUDE_DIR}"
                      "${CLANG_MAIN_INCLUDE_DIR}")
  link_directories("${LLVM_LIBRARY_DIR}"
                   "${PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/lib")

  set(LIT_ARGS_DEFAULT "-sv")
  if (MSVC OR XCODE)
    set(LIT_ARGS_DEFAULT "${LIT_ARGS_DEFAULT} --no-progress-bar")
  endif()
  set(LLVM_LIT_ARGS "${LIT_ARGS_DEFAULT}" CACHE STRING "Default options for lit")

  set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
  set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
  set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )

  set( LLVM_INCLUDE_TESTS ON )
  set( LLVM_INCLUDE_DOCS ON )

  set( SWIFT_BUILT_STANDALONE 1 )

else()
  set(PATH_TO_LLVM_SOURCE "${CMAKE_SOURCE_DIR}")
  set(PATH_TO_LLVM_BUILD "${CMAKE_BINARY_DIR}")
  set(SWIFT_PATH_TO_CLANG_BUILD "${CMAKE_BINARY_DIR}")
  set(PATH_TO_CLANG_BUILD "${SWIFT_PATH_TO_CLANG_BUILD}")
  set(CLANG_MAIN_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/tools/clang/include")
  set(CLANG_BUILD_INCLUDE_DIR "${CMAKE_BINARY_DIR}/tools/clang/include")

  if( NOT EXISTS "${CLANG_MAIN_INCLUDE_DIR}/clang/AST/Decl.h" )
    message(FATAL_ERROR "Clang is missing from llvm/tools subdirectory.")
  endif()

  include_directories("${CLANG_BUILD_INCLUDE_DIR}"
                      "${CLANG_MAIN_INCLUDE_DIR}")

  check_cxx_compiler_flag("-Werror -Wnested-anon-types" CXX_SUPPORTS_NO_NESTED_ANON_TYPES_FLAG)
  if( CXX_SUPPORTS_NO_NESTED_ANON_TYPES_FLAG )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-nested-anon-types" )
  endif()
endif()

# Add path for custom modules
set(CMAKE_MODULE_PATH
  ${CMAKE_MODULE_PATH}
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules"
  )

set(LLVM_RUNTIME_OUTPUT_INTDIR ${PATH_TO_LLVM_BUILD}/${CMAKE_CFG_INTDIR}/bin)
set(LLVM_LIBRARY_OUTPUT_INTDIR ${PATH_TO_LLVM_BUILD}/${CMAKE_CFG_INTDIR}/lib)

set(SWIFT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SWIFT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

set(SWIFT_RUNTIME_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin)
set(SWIFT_LIBRARY_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib)
set(SWIFT_INCLUDE_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/include)

option(SWIFT_OPTIMIZED "Enable optimization for Swift source files" YES)

# Disable stdlib internal checks in Release builds. Internal checks
# are intentionally enabled in RelWithDebInfo builds.
if(CMAKE_BUILD_TYPE STREQUAL "Release")
  set(SWIFT_STDLIB_INTERNAL_CHECKS_default NO)
else()
  set(SWIFT_STDLIB_INTERNAL_CHECKS_default YES)
endif()
option(
  SWIFT_STDLIB_INTERNAL_CHECKS "Enable internal checks for the Swift standard library"
  ${SWIFT_STDLIB_INTERNAL_CHECKS_default})

option(SWIFT_VERIFY_ALL "Run SIL verification after each transform when building Swift files in the build process" NO)
option(SWIFT_STDLIB_USE_ASSERT_CONFIG_RELEASE "Should the stdlib be build with assert config set to release" NO)
option(SWIFT_EMIT_SORTED_SIL "Sort SIL output by name to enable diffing of output" NO)
set(SWIFT_MODULE_CACHE_PATH "" CACHE PATH
  "Directory to use as the Clang module cache when building Swift source files")

set(SWIFT_API_NOTES_PATH "${SWIFT_SOURCE_DIR}/apinotes")
if(NOT EXISTS "${SWIFT_API_NOTES_PATH}/Foundation.apinotes")
  message(FATAL_ERROR "API notes are not available in ${SWIFT_API_NOTES_PATH}")
endif()

option(SWIFT_SPLIT_OBJC_SELECTORS 
       "Split Objective-C selectors when building the standard library" NO)

option(SWIFT_BUILD_STATIC 
       "Build static variants of the Swift runtime and SDK overlays"
       ${ENABLE_SWIFT_STATIC_RUNTIME})

option(SWIFT_BUILD_TOOLS "Build Swift tools" YES)
option(SWIFT_BUILD_STDLIB "Build Swift stdlib" YES)
set(SWIFT_RUN_LONG_TESTS OFF CACHE BOOL "Run tests that can take a long time to finish")
set(SWIFT_ASAN_BUILD OFF CACHE BOOL "This is an AddressSanitizer build")

set(SWIFT_EXTRA_FLAGS "" CACHE STRING
  "Extra flags to pass when compiling swift files")

set(SWIFT_DISTCC "" CACHE PATH
  "Path to distcc executable for distributed builds")

# Suppress a warning that this variable is unused in case the user
# doesn't ask for an iOS build
if(SWIFT_DISTCC) 
endif()

# Find the 'clang' executable for compiling API notes.
set (CLANG_APINOTES "" CACHE FILEPATH "Path to Clang executable to be used for compiling API notes")
if ( CLANG_APINOTES STREQUAL "" )
  if ( PATH_TO_CLANG_BUILD STREQUAL "" )
     set(CLANG_APINOTES "clang")
  else()
     set(CLANG_APINOTES "${PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/bin/clang")
  endif()
endif()

set(PATH_TO_SWIFT_BUILD "" CACHE FILEPATH "Path to existing Swift build")
if ( PATH_TO_SWIFT_BUILD STREQUAL "" )
   set(SWIFT_EXEC "swift")
else()
   set(SWIFT_EXEC "${PATH_TO_SWIFT_BUILD}/${CMAKE_CFG_INTDIR}/bin/swift")
endif()

# FIXME: unify with CLANG_HAVE_LIBXML, which is set in LLVM anyway.
find_package(LibXml2)
if(LIBXML2_FOUND)
  set(SWIFT_HAVE_LIBXML 1)
endif()

# Xcode: use libc++ and c++11 using proper build settings.
if ( XCODE )
  # Force usage of Clang.
  set(CMAKE_XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0"
      CACHE STRING "Xcode Compiler")
  # Use C++'11.
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LANGUAGE_STANDARD "c++11"
      CACHE STRING "Xcode C++ Language Standard")
  # Use libc++.
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LIBRARY "libc++"
      CACHE STRING "Xcode C++ Standard Library")
  # Enable some warnings not enabled by default.  These
  # mostly reset clang back to its default settings, since
  # Xcode passes -Wno... for many warnings that are not enabled
  # by default.
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_ABOUT_RETURN_TYPE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_ABOUT_MISSING_NEWLINE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_VALUE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_VARIABLE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_SIGN_COMPARE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_FUNCTION "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_HIDDEN_VIRTUAL_FUNCTIONS "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNINITIALIZED_AUTOS "YES")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_WARN_DOCUMENTATION_COMMENTS "YES")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_WARN_BOOL_CONVERSION "YES")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_WARN_EMPTY_BODY "YES")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_WARN_ENUM_CONVERSION "YES")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_WARN_INT_CONVERSION "YES")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_WARN_CONSTANT_CONVERSION "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_NON_VIRTUAL_DESTRUCTOR "YES")

  # Disable RTTI
  set(CMAKE_XCODE_ATTRIBUTE_GCC_ENABLE_CPP_RTTI "NO")

  # Disable exceptions
  set(CMAKE_XCODE_ATTRIBUTE_GCC_ENABLE_CPP_EXCEPTIONS "NO") 
endif ()

# Add additional warnings.
check_cxx_compiler_flag("-Werror -Wdocumentation" CXX_SUPPORTS_DOCUMENTATION_FLAG)
append_if(CXX_SUPPORTS_DOCUMENTATION_FLAG "-Wdocumentation" CMAKE_CXX_FLAGS)

check_cxx_compiler_flag("-Werror -Wimplicit-fallthrough" CXX_SUPPORTS_IMPLICIT_FALLTHROUGH_FLAG)
append_if(CXX_SUPPORTS_IMPLICIT_FALLTHROUGH_FLAG "-Wimplicit-fallthrough" CMAKE_CXX_FLAGS)

# Check for -Wunreachable-code-aggressive instead of -Wunreachable-code, as that indicates
# that we have the newer -Wunreachable-code implementation.
check_cxx_compiler_flag("-Werror -Wunreachable-code-aggressive" CXX_SUPPORTS_UNREACHABLE_CODE_FLAG)
append_if(CXX_SUPPORTS_UNREACHABLE_CODE_FLAG "-Wunreachable-code" CMAKE_CXX_FLAGS)

check_cxx_compiler_flag("-Werror -Woverloaded-virtual" CXX_SUPPORTS_OVERLOADED_VIRTUAL)
append_if(CXX_SUPPORTS_OVERLOADED_VIRTUAL "-Woverloaded-virtual" CMAKE_CXX_FLAGS)

# Check for '-fapplication-extension'.  On OSX/iOS we wish to link all
# dynamic libraries with this flag.
check_cxx_compiler_flag("-fapplication-extension" CXX_SUPPORTS_FAPPLICATION_EXTENSION)

# Set sizeof(void*) if cross-compiling.
if (CMAKE_CROSSCOMPILING AND NOT CMAKE_SIZEOF_VOID_P)
  if (CMAKE_SYSTEM_PROCESSOR MATCHES "64$")
    set(CMAKE_SIZEOF_VOID_P 8)
  else()
    set(CMAKE_SIZEOF_VOID_P 4)
  endif()
endif()

# Reset CMAKE_SYSTEM_PROCESSOR if not cross-compiling.
# CMake refuses to use `uname -m` on OS X 
# http://public.kitware.com/Bug/view.php?id=10326
if (NOT CMAKE_CROSSCOMPILING AND CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
  execute_process(COMMAND uname -m
                  OUTPUT_VARIABLE CMAKE_SYSTEM_PROCESSOR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

# Compile a swift file into an object file (as a library).
#
# Usage:
#   compile_swift_files(OUTPUT objfile  # Name of the resulting object file
#     SOURCES swift_src [swift_src...]  # Swift source files to compile
#     FLAGS -module-name foo            # Flags to add to the compilation
#     [SDK sdk]                         # SDK to build for
#     [ARCHITECTURE architecture]       # Architecture to build for
#     [DEPENDS cmake_target...]         # CMake targets on which the object 
#                                       # file depends.
#     [IS_MAIN]                         # This is an executable, not a library
#     [IS_STDLIB_CORE]                  # This is the core standard library
#     [OPT_FLAGS]                       # Optimization flags (overrides SWIFT_OPTIMIZE)
#     [STATIC]                          # This is a static library
#     [MODULE_DIR]                      # Put .swiftmodule, .swiftdoc., and .o
#                                       # into this directory.
#     [INSTALL]                         # Install produced files.
#     )
function(compile_swift_files)
  parse_arguments(SWIFTFILE
    "OUTPUT;SOURCES;FLAGS;DEPENDS;SDK;ARCHITECTURE;API_NOTES;OPT_FLAGS;MODULE_DIR"
    "IS_MAIN;IS_STDLIB_CORE;STATIC;INSTALL"
    ${ARGN})
  
  if(SWIFTFILE_IS_MAIN AND SWIFTFILE_IS_STDLIB_CORE)
    error(SEND_ERROR "Cannot set both IS_MAIN and IS_STDLIB_CORE")
  endif()
  
  set(swift_flags ${SWIFTFILE_FLAGS})

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
  if(NOT SWIFTFILE_OPT_FLAGS)
    if(SWIFT_OPTIMIZED)
      list(APPEND swift_flags "-O")
    else()
      list(APPEND swift_flags "-Onone")
    endif()
  else()
    list(APPEND swift_flags "${SWIFTFILE_OPT_FLAGS}")
  endif()

  if (SWIFT_SPLIT_OBJC_SELECTORS)
    list(APPEND swift_flags "-split-objc-selectors")
  endif()

  # FIXME: We shouldn't /have/ to build things in a single process.
  # <rdar://problem/15972329>
  list(APPEND swift_flags "-force-single-frontend-invocation")

  # Don't include libarclite in any build products by default.
  list(APPEND swift_flags "-no-link-objc-runtime")

  if (SWIFT_STDLIB_INTERNAL_CHECKS)
    list(APPEND swift_flags "-Xfrontend" "-D" "-Xfrontend" "INTERNAL_CHECKS_ENABLED")
  endif()

  if(SWIFT_VERIFY_ALL)
    list(APPEND swift_flags "-Xfrontend" "-sil-verify-all")
  endif()

  if (SWIFT_STDLIB_USE_ASSERT_CONFIG_RELEASE)
    list(APPEND swift_flags "-assert-config" "Release")
  endif()

  if (SWIFT_EMIT_SORTED_SIL_OUTPUT)
    list(APPEND swift_flags "-Xfrontend" "-emit-sorted-sil")
  endif()

  # Allow import of other Swift modules we just built.
  list(APPEND swift_flags "-resource-dir" "${SWIFTLIB_DIR}")

  # Turn on debug symbols in appropriate builds.
  if(CMAKE_BUILD_TYPE MATCHES "Debug|.*DebInfo.*")
    list(APPEND swift_flags "-g")
  endif()
  
  if (SWIFTFILE_SDK)
    # If we have an SDK, we're building for a target.
    list(APPEND swift_flags "-sdk" "${SWIFT_SDK_${SWIFTFILE_SDK}_PATH}")

    # Determine the triple from the SDK and architecture.
    list(APPEND swift_flags "-target" 
      "${SWIFTFILE_ARCHITECTURE}-apple-${SWIFT_SDK_${SWIFTFILE_SDK}_TRIPLE_NAME}${SWIFT_SDK_${SWIFTFILE_SDK}_DEPLOYMENT_VERSION}")

    # Determine the subdirectory where the binary should go.
    set(SWIFTFILE_LIBRARY_SUBDIR ${SWIFT_SDK_${SWIFTFILE_SDK}_LIB_SUBDIR})
    if (NOT ${SWIFTFILE_ARCHITECTURE} MATCHES "64")
      set(SWIFTFILE_LIBRARY_SUBDIR "${SWIFTFILE_LIBRARY_SUBDIR}/32")
    endif()
  else()
    # We're building for the host.

    # If we have a modules SDK, use it for building.
    if (MODULES_SDK)
      list(APPEND swift_flags "-sdk" "${MODULES_SDK}")
    endif()

    # If we have a host triple, use it.
    if (SWIFT_HOST_TRIPLE)
      list(APPEND swift_flags "-target" "${SWIFT_HOST_TRIPLE}")
    endif()

    # Determine the subdirectory where the binary should go.
    set(arch_subdir "")
    if(NOT SWIFT_HOST_TRIPLE MATCHES "64")
      set(arch_subdir "/32")
    endif()
    
    set(SWIFTFILE_LIBRARY_SUBDIR "${SWIFTLIB_SUBDIR}${arch_subdir}")
  endif()

  # If we have a custom module cache path, use it.
  if (SWIFT_MODULE_CACHE_PATH)
    list(APPEND swift_flags "-module-cache-path" "${SWIFT_MODULE_CACHE_PATH}")
  endif()

  list(APPEND swift_flags ${SWIFT_EXTRA_FLAGS})

  # FIXME: Cleaner way to do this?

  if(NOT SWIFTFILE_IS_MAIN)
    list(APPEND swift_flags -parse-as-library)
  endif()
  if(SWIFTFILE_IS_STDLIB_CORE)
    list(APPEND swift_flags -nostdimport -parse-stdlib -module-name "Swift" -Xfrontend -sil-serialize-all)
  endif()

  set(objfile ${SWIFTFILE_OUTPUT})
  if (NOT IS_ABSOLUTE ${objfile})
    # If we have an SDK, put the object file into a subdirectory that
    # identifies where the library would go.
    set(objsubdir)
    if (SWIFTFILE_SDK)
      set(objsubdir "/${SWIFTFILE_LIBRARY_SUBDIR}")
    elseif(SWIFTFILE_MODULE_DIR)
      set(objsubdir "/${SWIFTFILE_MODULE_DIR}")
    endif()

    set(objfile
      "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}/${objfile}")
    file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}")
  endif()

  set(create_apinotes)
  set(apinote_files)

  if(SWIFTFILE_IS_MAIN)
    set(module_file)
    set(create_module_dir)
  else()
    get_filename_component(module_name ${objfile} NAME_WE)

    if(NOT SWIFTFILE_MODULE_DIR)
      set(module_dir "${SWIFTLIB_DIR}/${SWIFTFILE_LIBRARY_SUBDIR}")
      if(SWIFTFILE_STATIC)
        set(module_dir "${module_dir}/static")
      endif()
    else()
      set(module_dir "${SWIFTFILE_MODULE_DIR}")
    endif()

    set(module_file ${module_dir}/${module_name}.swiftmodule)
    set(module_doc_file ${module_dir}/${module_name}.swiftdoc)
    list(APPEND swift_flags -emit-module -emit-module-path ${module_file})

    set(create_module_dir
      COMMAND ${CMAKE_COMMAND} -E make_directory ${module_dir})

    # If we're supposed to generate API notes, do so.
    foreach(apinote_module ${SWIFTFILE_API_NOTES})
      set(apinote_file "${module_dir}/${apinote_module}.apinotesc")
      set(apinote_input_file
        "${SWIFT_API_NOTES_PATH}/${apinote_module}.apinotes")

      # Figure out the target to pass to create API notes.
      if (SWIFTFILE_SDK)
        set(apinote_target "-target" "${SWIFTFILE_ARCHITECTURE}-apple-${SWIFT_SDK_${SWIFTFILE_SDK}_TRIPLE_NAME}${SWIFT_SDK_${SWIFTFILE_SDK}_DEPLOYMENT_VERSION}")
      elseif(SWIFT_HOST_TRIPLE)
        set(apinote_target "-target" "${SWIFT_HOST_TRIPLE}")
      else()
        set(apinote_target)
      endif()
        
      list(APPEND create_apinotes
        COMMAND ${CLANG_APINOTES} -cc1apinotes -yaml-to-binary -o ${apinote_file} ${apinote_target} ${apinote_input_file}
        DEPENDS ${apinote_input_file})

      list(APPEND apinote_files ${apinote_file})
      install(FILES ${apinote_file}
        DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFTFILE_LIBRARY_SUBDIR})
    endforeach()

    if (SWIFTFILE_INSTALL)
      install(FILES ${module_file} ${module_doc_file}
        DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFTFILE_LIBRARY_SUBDIR})
    endif()
  endif()

  set(stdlib_dep)
  # Make sure that standard library does not depend on itself.
  if(NOT SWIFTFILE_IS_STDLIB_CORE)
    set(stdlib_dep swiftCore)
  endif()

  set(line_directive_tool "${SWIFT_SOURCE_DIR}/utils/line-directive")

  # swiftc
  if (TARGET "${SWIFT_EXEC}")
    set(swift_compiler_path "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${SWIFT_EXEC}c")
  else()
    set(swift_compiler_path "${SWIFT_EXEC}c")
  endif()

  add_custom_command(
    OUTPUT ${objfile} ${module_file} ${module_doc_file} ${apinote_files}
    ${create_module_dir}
    ${create_apinotes}
    COMMAND "${line_directive_tool}" ${source_files} -- 
      "${swift_compiler_path}" "-c" ${swift_flags} "-o" "${objfile}" ${source_files}
    DEPENDS "${SWIFT_EXEC}" ${stdlib_dep} ${source_files} ${SWIFTFILE_DEPENDS} ${swift_ide_test_dependency}
#    MAIN_DEPENDENCY ${target}
    COMMENT "Compiling ${objfile}")
  
  # Make sure the build system knows the file is a generated object file.
  set_source_files_properties(${objfile}
    PROPERTIES 
    GENERATED true 
    EXTERNAL_OBJECT true
    LANGUAGE C
    OBJECT_DEPENDS "${source_files}")
endfunction()

# Compute the library subdirectory to use for the given sdk and
# architecture, placing the result in 'resultvar'.
function(compute_library_subdir resultvar sdk architecture)
  set(result ${SWIFT_SDK_${sdk}_LIB_SUBDIR})
  if (NOT ${architecture} MATCHES "64")
    set(result "${result}/32")
  endif()

  set(${resultvar} "${result}" PARENT_SCOPE)
endfunction(compute_library_subdir)

# Process the sources within the given variable, pulling out any Swift
# sources to be compiled with 'swift' directly. This updates
# ${sourcesvar} in place with the resulting list and ${externalvar} with the
# list of externally-build sources.
#
# Usage:
#   handle_swift_sources(sourcesvar externalvar)
function(handle_swift_sources sourcesvar externalvar name)
  parse_arguments(SWIFTSOURCES  
    "DEPENDS;SDK;ARCHITECTURE;API_NOTES;COMPILE_FLAGS;OPT_FLAGS"
    "IS_MAIN;IS_STDLIB_CORE;STATIC;INSTALL"
    ${ARGN})

  set(result)
  set(swift_sources)
  set(asm_sources)
  foreach(src ${${sourcesvar}})
    get_filename_component(extension ${src} EXT)
    if(extension STREQUAL ".swift")
      list(APPEND swift_sources ${src})
    else()
      list(APPEND result ${src})
    endif()
  endforeach()

  set(swift_compile_flags ${SWIFTSOURCES_COMPILE_FLAGS})
  if (NOT SWIFTSOURCES_IS_MAIN)
    list(APPEND swift_compile_flags -module-link-name "${name}")
  endif()
  translate_flag(${SWIFTSOURCES_IS_MAIN} "IS_MAIN" IS_MAIN_arg)
  translate_flag(${SWIFTSOURCES_IS_STDLIB_CORE} "IS_STDLIB_CORE"
                 IS_STDLIB_CORE_arg)
  translate_flag(${SWIFTSOURCES_STATIC} "STATIC" STATIC_arg)
  translate_flag(${SWIFTSOURCES_INSTALL} "INSTALL" INSTALL_arg)

  if(swift_sources)
    # Special-case hack to create Swift.o for the core standard library.
    if(SWIFTSOURCES_IS_STDLIB_CORE)
      set(swift_obj_base "Swift")
    else()
      # Otherwise, get the name from the first swift input file.  Also
      # a hack!
      # TODO: Fix it, <rdar://problem/17535693>.
      list(GET swift_sources 0 swift_obj_base)
      get_filename_component(swift_obj_base ${swift_obj_base} NAME_WE)
    endif()

    set(objsubdir)
    if (SWIFTSOURCES_SDK)
      compute_library_subdir(SWIFTSOURCES_LIBRARY_SUBDIR
        ${SWIFTSOURCES_SDK} ${SWIFTSOURCES_ARCHITECTURE})
      set(objsubdir "/${SWIFTSOURCES_LIBRARY_SUBDIR}")
    endif()

    if(SWIFTSOURCES_STATIC)
      set(objsubdir "${objsubdir}/static")
    endif()

    file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}")

    set(swift_obj 
        "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}/${swift_obj_base}${CMAKE_C_OUTPUT_EXTENSION}")

    compile_swift_files(OUTPUT ${swift_obj}
      SOURCES ${swift_sources}
      DEPENDS ${SWIFTSOURCES_DEPENDS}
      FLAGS ${swift_compile_flags}
      SDK ${SWIFTSOURCES_SDK}
      ARCHITECTURE ${SWIFTSOURCES_ARCHITECTURE}
      API_NOTES ${SWIFTSOURCES_API_NOTES}
      ${IS_MAIN_arg} ${IS_STDLIB_CORE_arg} ${STATIC_arg}
      OPT_FLAGS ${SWIFTSOURCES_OPT_FLAGS}
      ${INSTALL_arg})
    list(APPEND result ${swift_obj})
  endif()

  llvm_process_sources(result ${result})
  set(${sourcesvar} "${result}" PARENT_SCOPE)
  set(${externalvar} ${swift_sources} ${asm_sources} PARENT_SCOPE)
endfunction()

# Set SWIFTLIB_DIR to the directory in the build tree where Swift resource files
# should be placed. Note that $CMAKE_CFG_INTDIR expands to "." for
# single-configuration builds.
set(SWIFTLIB_DIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift")
set(SWIFTSTATICLIB_DIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift_static")

# Set SWIFTLIB_SUBDIR to the subdirectory inside SWIFTLIB_DIR 
# where Swift .swiftmodules and libraries should be placed.
# This is changed later based on SWIFT_DEPLOYMENT_OS.
# The OS is currently needed in lieu of proper SDK support.
set(SWIFTLIB_SUBDIR ".")

# Configuration flags passed to all of our invocations of gyb.  Try to
# avoid making up new variable names here if you can find a CMake
# variable that will do the job.
set(SWIFT_GYB_FLAGS
  "-DunicodeGraphemeBreakPropertyFile=${SWIFT_SOURCE_DIR}/utils/UnicodeData/GraphemeBreakProperty.txt"
  "-DunicodeGraphemeBreakTestFile=${SWIFT_SOURCE_DIR}/utils/UnicodeData/GraphemeBreakTest.txt"
  "--test" # Run gyb's self-tests whenever we use it.  They're cheap
           # enough and it keeps us honest.
  )

# replace, in ${sourcesvar}, the given .gyb-suffixed sources with
# their un-suffixed intermediate files, which will be generated by
# processing the .gyb files with gyb.
# If the option STATIC is provided, produce the output in a subdir named 'static'.
function(handle_gyb_sources ptr_size sourcesvar)
  parse_arguments(GYBFILE "" "STATIC" ${ARGN})
  set(de_gybbed_sources)
  set(gyb_sources)
  set(gyb_tool "${SWIFT_SOURCE_DIR}/utils/gyb")
  set(gyb_extra_sources "${SWIFT_SOURCE_DIR}/utils/GYBUnicodeDataUtils.py")
  foreach (src ${${sourcesvar}})
    string(REGEX REPLACE "[.]gyb$" "" src_sans_gyb "${src}")
    if(src STREQUAL src_sans_gyb)
      list(APPEND de_gybbed_sources "${src}")
    else()
      set(dir "${CMAKE_CURRENT_BINARY_DIR}/${ptr_size}")
      if(GYBFILE_STATIC)
        set(dir "${dir}/static")
      endif()
      file(MAKE_DIRECTORY "${dir}")
      set(output_file "${dir}/${src_sans_gyb}")
      list(APPEND de_gybbed_sources "${output_file}")
      add_custom_command(
        OUTPUT "${output_file}"
        COMMAND "${gyb_tool}"  ${SWIFT_GYB_FLAGS} -o "${output_file}" "${src}"
          "-DCMAKE_SIZEOF_VOID_P=${ptr_size}"
        DEPENDS "${gyb_tool}" "${src}" "${gyb_extra_sources}"
        COMMENT "Generating ${src_sans_gyb} from ${src} with ptr size = ${ptr_size}"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        )
    endif()
  endforeach()
  set(${sourcesvar} "${de_gybbed_sources}" PARENT_SCOPE)
endfunction()

# Add a universal binary target created from the output of the given
# set of targets by running 'lipo'.
#
# Usage:
#   add_swift_lipo_target(
#     target              # The name of the target to create
#     output              # The file to be created by this target
#     source_targets...   # The source targets whose outputs will be
#                         # lipo'd into the output.
#   )
function(add_swift_lipo_target target output)
  set(source_targets ${ARGN})

  # Gather the source binaries.
  set(source_binaries)
  foreach(source_target ${source_targets})
    get_property(source_binary TARGET ${source_target} PROPERTY LOCATION)
    list(APPEND source_binaries "${source_binary}")
  endforeach()

  # Use lipo to create the final binary.
  add_custom_target(${target} ALL
    COMMAND ${LIPO}
    "-create"
    "-output"
    ${output}
    ${source_binaries}
    DEPENDS ${source_targets})
endfunction()

# Add a single variant of a new Swift library.
#
# Usage:
#   add_swift_library_single(
#     target                         # Name of the target
#                                    #   (e.g., swiftParse-IOS-armv7)
#     name                           # Name of the library (e.g., swiftParse)
#     [SDK sdk]                      # SDK to build for
#     [ARCHITECTURE architecture]    # Architecture to build for
#     [DEPENDS dep1 ...]             # Libraries this library depends on
#     [FRAMEWORK_DEPENDS dep1 ...]   # System frameworks this library depends on
#     [COMPONENT_DEPENDS comp1 ...]  # LLVM components this library depends on
#     [C_COMPILE_FLAGS flag1...]     # Extra compile flags (C/C++/ObjC)
#     [SWIFT_COMPILE_FLAGS flag1...] # Extra compile flags (Swift)
#     [LINK_FLAGS flag1...]          # Extra linker flags
#     [API_NOTES_NON_OVERLAY]        # Generate API notes for non-overlayed 
#                                    # modules with this target.
#     [FILE_DEPENDS target1 ...]     # Additional files this library depends on
#     [INSTALL]                      # Install library to lib/swift
#     [IS_STDLIB_CORE]               # Compile as the standard library core
#     [NO_LLVM_COMMON_LIBS]          # Turn off linking of LLVM Common Libs.
#     source1 [source2 source3 ...]) # Sources to add into this library
macro(add_swift_library_single target name)
  parse_arguments(SWIFTLIB_SINGLE
    "DEPENDS;FRAMEWORK_DEPENDS;COMPONENT_DEPENDS;C_COMPILE_FLAGS;SWIFT_COMPILE_FLAGS;LINK_FLAGS;FILE_DEPENDS;SDK;ARCHITECTURE"
    "INSTALL;IS_STDLIB_CORE;NO_LLVM_COMMON_LIBS;API_NOTES_NON_OVERLAY"
    ${ARGN})

  set(SWIFTLIB_SINGLE_SOURCES ${SWIFTLIB_SINGLE_DEFAULT_ARGS})

  # Determine the subdirectory where this library will be installed.
  if (SWIFTLIB_SINGLE_SDK)
    set(SWIFTLIB_SINGLE_SUBDIR "${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}/${SWIFTLIB_SINGLE_ARCHITECTURE}")
    if (${SWIFTLIB_SINGLE_ARCHITECTURE} MATCHES "64")
      set(SWIFTLIB_SINGLE_PTR_SIZE 8)
    else()
      set(SWIFTLIB_SINGLE_PTR_SIZE 4)
    endif()
  else()
    set(SWIFTLIB_SINGLE_SUBDIR ${SWIFTLIB_SUBDIR})
    set(SWIFTLIB_SINGLE_PTR_SIZE ${CMAKE_SIZEOF_VOID_P})
  endif()
  
  if(MSVC_IDE OR XCODE)
    string( REGEX MATCHALL "/[^/]+" split_path ${CMAKE_CURRENT_SOURCE_DIR})
    list( GET split_path -1 dir)
    file( GLOB_RECURSE SWIFTLIB_SINGLE_HEADERS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.h
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.def)

    file( GLOB_RECURSE SWIFTLIB_SINGLE_TDS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.td)
    source_group("TableGen descriptions" FILES ${SWIFTLIB_SINGLE_TDS})

    set(SWIFTLIB_SINGLE_SOURCES ${SWIFTLIB_SINGLE_SOURCES} ${SWIFTLIB_SINGLE_HEADERS} ${SWIFTLIB_SINGLE_TDS})
  endif(MSVC_IDE OR XCODE)

  if (MODULE)
    set(libkind MODULE)
  elseif (SHARED_LIBRARY)
    set(libkind SHARED)
  else()
    set(libkind)
  endif()

  set(IS_STDLIB_CORE_arg)
  if(SWIFTLIB_SINGLE_IS_STDLIB_CORE)
    set(IS_STDLIB_CORE_arg IS_STDLIB_CORE)
  endif()
  if(SWIFTLIB_SINGLE_INSTALL)
    set(INSTALL_arg INSTALL)
  endif()

  # Copy the sources now before they may get modified.
  set(SWIFTLIB_SINGLE_STATIC_SOURCES ${SWIFTLIB_SINGLE_SOURCES})

  handle_gyb_sources(${SWIFTLIB_SINGLE_PTR_SIZE} SWIFTLIB_SINGLE_SOURCES)
  
  # Figure out whether and which API notes to create.
  set(SWIFTLIB_SINGLE_API_NOTES)
  if (SWIFTLIB_SINGLE_API_NOTES_NON_OVERLAY)
    # Adopt all of the non-overlay API notes.
    foreach(next_notes_input ${SWIFT_API_NOTES_INPUTS})
      if (NOT IS_DIRECTORY "${SWIFT_SOURCE_DIR}/stdlib/objc/${next_notes_input}")
        list(APPEND SWIFTLIB_SINGLE_API_NOTES "${next_notes_input}")
      endif()
    endforeach()
  endif()

  # Remove the "swift" prefix from the name to determine the module
  # name.
  string(REPLACE swift "" module_name ${name})
  list(FIND SWIFT_API_NOTES_INPUTS ${module_name} overlay_index)
  if (${overlay_index} GREATER -1)
    set(SWIFTLIB_SINGLE_API_NOTES ${module_name})
  endif()

  # FIXME: don't actually depend on the libraries in SWIFTLIB_SINGLE_DEPENDS,
  # just any swiftmodule files that are associated with them.
  handle_swift_sources(SWIFTLIB_SINGLE_SOURCES 
    SWIFTLIB_SINGLE_EXTERNAL_SOURCES ${name}
    DEPENDS ${SWIFTLIB_SINGLE_FILE_DEPENDS} ${SWIFTLIB_SINGLE_DEPENDS}
    SDK ${SWIFTLIB_SINGLE_SDK}
    ARCHITECTURE ${SWIFTLIB_SINGLE_ARCHITECTURE}
    API_NOTES ${SWIFTLIB_SINGLE_API_NOTES}
    COMPILE_FLAGS ${SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS}
    ${IS_STDLIB_CORE_arg} ${INSTALL_arg})

  add_library( ${target} ${libkind}
    ${SWIFTLIB_SINGLE_SOURCES}
    ${SWIFTLIB_SINGLE_EXTERNAL_SOURCES})
  llvm_update_compile_flags(${target})

  set_source_files_properties(
    ${SWIFTLIB_SINGLE_SOURCES}
    PROPERTIES OBJECT_DEPENDS "${SWIFTLIB_SINGLE_FILE_DEPENDS}")
  
  set_output_directory(${target} ${SWIFT_RUNTIME_OUTPUT_INTDIR} ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  # Collect compile and link flags for the static and non-static targets.
  # Don't set PROPERTY COMPILE_FLAGS or LINK_FLAGS directly.
  set(c_compile_flags "${SWIFTLIB_SINGLE_C_COMPILE_FLAGS}")
  set(link_flags "${SWIFTLIB_SINGLE_LINK_FLAGS}")

  if (MODULE)
    set_target_properties(${target} PROPERTIES
      PREFIX ""
      SUFFIX ${LLVM_PLUGIN_EXT}
      )
  endif()

  # Add Swift source files to the project.
  source_group("Swift Sources" FILES ${SWIFTLIB_SINGLE_EXTERNAL_SOURCES})
  # ...and for now, mark them as HEADER_FILE_ONLY, so that Xcode doesn't try
  # to build them itself.
  set_source_files_properties(${SWIFTLIB_SINGLE_EXTERNAL_SOURCES} PROPERTIES
    HEADER_FILE_ONLY true)

  if(SWIFTLIB_SINGLE_INSTALL)
    # Install runtime libraries to lib/swift instead of lib. This works around
    # the fact that -isysroot prevents linking to libraries in the system
    # /usr/lib if Swift is installed in /usr.
    set_target_properties(${target} PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR} )

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
      set_target_properties(${target} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR}
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR})
    endforeach()

    install(TARGETS ${target}
      LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFTLIB_SINGLE_SUBDIR})
  endif(SWIFTLIB_SINGLE_INSTALL)

  # Add variant-specific flags.
  if (SWIFTLIB_SINGLE_SDK)
    set(SWIFTLIB_SINGLE_DEPLOYMENT_FLAGS
        "-m${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_VERSION_MIN_NAME}-version-min=${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_DEPLOYMENT_VERSION}")
    set(link_flags "${link_flags} ${SWIFTLIB_SINGLE_DEPLOYMENT_FLAGS} -arch ${SWIFTLIB_SINGLE_ARCHITECTURE} -isysroot ${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_PATH}")
    # SDK, deployment target, architecture.
    set(c_compile_flags
      "${c_compile_flags} ${SWIFTLIB_SINGLE_DEPLOYMENT_FLAGS} -arch ${SWIFTLIB_SINGLE_ARCHITECTURE} -isysroot ${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_PATH}")
    set_target_properties(${target}
      PROPERTIES
      # Library name (without the variant information)
      OUTPUT_NAME ${name})
  else()
    if (SWIFT_SDKS)
      # Deployment flags.
      set(c_compile_flags "${c_compile_flags} ${SWIFT_GLOBAL_DEPLOYMENT_TARGET_FLAGS}")
      set(link_flags "${link_flags} ${SWIFT_GLOBAL_DEPLOYMENT_TARGET_FLAGS}")
    endif()
  endif()

  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set_target_properties(${target}
      PROPERTIES
      INSTALL_NAME_DIR "@rpath"
      BUILD_WITH_INSTALL_RPATH YES)
  endif()
  
  # Configure plist creation for OS X.
  set (PLIST_INFO_PLIST "Info.plist" CACHE STRING "Plist name")
  if (APPLE AND SHARED_LIBRARY)
    set(PLIST_INFO_NAME ${name})
    set(PLIST_INFO_UTI "com.apple.dt.runtime.${name}")
    set(PLIST_INFO_VERSION "1.1")
    if (LLVM_SUBMIT_VERSION)
      set(PLIST_INFO_BUILD_VERSION
        "${LLVM_SUBMIT_VERSION}.${LLVM_SUBMIT_SUBVERSION}")
    endif()

    set(PLIST_INFO_PLIST_OUT "${PLIST_INFO_PLIST}")
    set(link_flags "${link_flags} -Wl,-sectcreate,__TEXT,__info_plist,${CMAKE_CURRENT_BINARY_DIR}/${PLIST_INFO_PLIST_OUT}")
    configure_file("${SWIFT_SOURCE_DIR}/stdlib/${PLIST_INFO_PLIST}.in" "${PLIST_INFO_PLIST_OUT}" @ONLY)

    # If Application Extensions are enabled, pass the linker flag marking
    # the dylib as safe.
    if (CXX_SUPPORTS_FAPPLICATION_EXTENSION AND (NOT DISABLE_APPLICATION_EXTENSION))
      set(link_flags "${link_flags} -Wl,-application_extension")
    endif()

    set(PLIST_INFO_UTI)
    set(PLIST_INFO_NAME)
    set(PLIST_INFO_VERSION)
    set(PLIST_INFO_BUILD_VERSION)
  endif()

  set_target_properties(${target} PROPERTIES FOLDER "Swift libraries")

  # Configure the static library target.
  # Set compile and link flags for the non-static target. 
  # Do these LAST.

  # FIXME: Should check whether this is a target library. It has nothing to
  # do with installation.
  if (SWIFT_BUILD_STATIC AND SWIFTLIB_SINGLE_INSTALL)
    set(target_static "${target}-static")

    handle_gyb_sources(${SWIFTLIB_SINGLE_PTR_SIZE} SWIFTLIB_SINGLE_STATIC_SOURCES STATIC)

    handle_swift_sources(SWIFTLIB_SINGLE_STATIC_SOURCES
      SWIFTLIB_SINGLE_STATIC_EXTERNAL_SOURCES ${name}
      DEPENDS ${SWIFTLIB_SINGLE_FILE_DEPENDS} ${SWIFTLIB_SINGLE_DEPENDS}
      SDK ${SWIFTLIB_SINGLE_SDK}
      ARCHITECTURE ${SWIFTLIB_SINGLE_ARCHITECTURE}
      API_NOTES ${SWIFTLIB_SINGLE_API_NOTES}
      COMPILE_FLAGS ${SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS}
      ${IS_STDLIB_CORE_arg} ${INSTALL_arg}
      STATIC)

    # Note: the dummy.c source file provides no definitions. However,
    # it forces Xcode to properly link the static library.
    add_library( ${target_static} STATIC
      ${SWIFTLIB_SINGLE_STATIC_SOURCES}
      ${SWIFTLIB_SINGLE_STATIC_EXTERNAL_SOURCES}
      ${SWIFT_SOURCE_DIR}/cmake/dummy.c)
    set_target_properties(${target_static} PROPERTIES OUTPUT_NAME ${name})

    set_output_directory(${target_static}
      ${SWIFT_RUNTIME_OUTPUT_INTDIR} ${SWIFT_LIBRARY_OUTPUT_INTDIR})

    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER ${config} config_upper)
      # Hack to deal with the fact that ${SWIFTLIB_DIR} contains the build-time
      # variable $(CONFIGURATION). Note that this fix is Xcode-specific.
      # You might think you could just replace ${CMAKE_CFG_INTDIR} here, but
      # that can actually have other things mangled into it besides just the
      # configuration. This at least doesn't pretend to do the right thing
      # outside of Xcode.
      string(REPLACE "$(CONFIGURATION)" ${config} config_lib_dir
        ${SWIFTSTATICLIB_DIR})
      set_target_properties(${target_static} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR}
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR})
    endforeach()
    
    install(TARGETS ${target_static}
      LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift_static/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX}/swift_static/${SWIFTLIB_SINGLE_SUBDIR})

    set_target_properties(${target_static} PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${SWIFTSTATICLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE_OUTPUT_DIRECTORY ${SWIFTSTATICLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR} )
  else()
    unset(target_static)
  endif()

  # Handle linking and dependencies.
  target_link_libraries(${target} ${SWIFTLIB_SINGLE_DEPENDS})
  if (target_static AND SWIFTLIB_SINGLE_DEPENDS)
    set(depends)
    foreach (dep ${SWIFTLIB_SINGLE_DEPENDS})
      list(APPEND depends "${dep}-static")
    endforeach()
    add_dependencies(${target_static} ${depends})
  endif()

  if( LLVM_COMMON_DEPENDS )
    add_dependencies( ${target} ${LLVM_COMMON_DEPENDS} )
    if (target_static)
      add_dependencies( ${target_static} ${LLVM_COMMON_DEPENDS} )
    endif()
  endif( LLVM_COMMON_DEPENDS )

  # Link against system frameworks.
  foreach(FRAMEWORK ${SWIFTLIB_SINGLE_FRAMEWORK_DEPENDS})
    target_link_libraries(${target} "-framework ${FRAMEWORK}")
    if (target_static)
      target_link_libraries(${target_static} "-framework ${FRAMEWORK}")
    endif()
  endforeach()

  llvm_config( ${target} ${SWIFTLIB_SINGLE_COMPONENT_DEPENDS} )
  if (NOT NO_LLVM_COMMON_LIBS)
    target_link_libraries( ${target} ${LLVM_COMMON_LIBS} )
    if (target_static AND LLVM_COMMON_LIBS)
      add_dependencies( ${target_static} ${LLVM_COMMON_LIBS} )
    endif()
  endif() 

  # Set compilation and link flags.
  set_property(TARGET ${target} APPEND_STRING PROPERTY
    COMPILE_FLAGS " ${c_compile_flags}")
  set_property(TARGET ${target} APPEND_STRING PROPERTY
    LINK_FLAGS " ${link_flags} -L${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}")

  if (target_static)
    set_property(TARGET ${target_static} APPEND_STRING PROPERTY
      COMPILE_FLAGS " ${c_compile_flags}")
    set_property(TARGET ${target_static} APPEND_STRING PROPERTY
      LINK_FLAGS " ${link_flags} -L${SWIFTSTATICLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}")
  endif()

  # Do not add code here.
endmacro(add_swift_library_single)

# Translate a yes/no variable to the presence of a given string in a
# variable.
#
# Usage:
#   translate_flag(is_set flag_name var_name)
#
# If is_set is true, sets ${var_name} to ${flag_name}. Otherwise,
# unsets ${var_name}.
macro(translate_flag IS_SET FLAG_NAME VAR_NAME)
  if (${IS_SET})
    set(${VAR_NAME} ${FLAG_NAME})
  else()
    unset(${VAR_NAME})
  endif()
endmacro(translate_flag)

# Add a new Swift library.
#
# Usage:
#   add_swift_library(name           # Name of the library (e.g., swiftParse)
#     [DEPENDS dep1 ...]             # Libraries this library depends on
#     [FRAMEWORK_DEPENDS dep1 ...]   # System frameworks this library depends on
#     [COMPONENT_DEPENDS comp1 ...]  # LLVM components this library depends on
#     [FILE_DEPENDS target1 ...]     # Additional files this library depends on
#     [TARGET_SDKS sdk1...]          # The set of SDKs in which this library
#                                    # is included. If empty, the library is
#                                    # included in all SDKs.
#     [C_COMPILE_FLAGS flag1...]     # Extra compile flags (C/C++/ObjC)
#     [SWIFT_COMPILE_FLAGS flag1...] # Extra compile flags (Swift)
#     [LINK_FLAGS flag1...]          # Extra linker flags
#     [API_NOTES_NON_OVERLAY]        # Generate API notes for non-overlayed 
#                                    # modules with this target.
#     [INSTALL]                      # Install library to lib/swift
#     [IS_STDLIB_CORE]               # Compile as the standard library core
#     [NO_LLVM_COMMON_LIBS]          # Turn off linking of LLVM Common Libs.
#     [TARGET_LIBRARY]               # Build library for the target SDKs
#     source1 [source2 source3 ...]) # Sources to add into this library
macro(add_swift_library name)
  parse_arguments(SWIFTLIB 
    "DEPENDS;FRAMEWORK_DEPENDS;COMPONENT_DEPENDS;FILE_DEPENDS;TARGET_SDKS;C_COMPILE_FLAGS;SWIFT_COMPILE_FLAGS;LINK_FLAGS"
    "INSTALL;IS_STDLIB_CORE;NO_LLVM_COMMON_LIBS;TARGET_LIBRARY;API_NOTES_NON_OVERLAY"
    ${ARGN})
  set(SWIFTLIB_SOURCES ${SWIFTLIB_DEFAULT_ARGS}) 

  # Translate flags.
  translate_flag(${SWIFTLIB_INSTALL} "INSTALL" SWIFTLIB_INSTALL_FLAG)
  translate_flag(${SWIFTLIB_IS_STDLIB_CORE} "IS_STDLIB_CORE" 
                 SWIFTLIB_IS_STDLIB_CORE_FLAG)
  translate_flag(${SWIFTLIB_NO_LLVM_COMMON_LIBS} "NO_LLVM_COMMON_LIBS" 
                 SWIFTLIB_NO_LLVM_COMMON_LIBS_FLAG)
  translate_flag(${SWIFTLIB_API_NOTES_NON_OVERLAY}
                 "API_NOTES_NON_OVERLAY" 
                 SWIFTLIB_API_NOTES_NON_OVERLAY_FLAG)

  set(SWIFTLIB_DEFAULT_C_COMPILE_FLAGS)
  if (SWIFT_OPTIMIZED AND SWIFTLIB_TARGET_LIBRARY)
    if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
      set(SWIFTLIB_DEFAULT_C_COMPILE_FLAGS "-momit-leaf-frame-pointer")
    elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
      set(SWIFTLIB_DEFAULT_C_COMPILE_FLAGS "-momit-leaf-frame-pointer")
    endif()
  endif()

  # Use 16K segment alignment on 32-bit arm.
  # The linker does this by default for iOS 8+ deployment, 
  # but we deploy to iOS 7.
  set(SWIFTLIB_DEFAULT_LINK_FLAGS)
  if (CMAKE_SYSTEM_PROCESSOR MATCHES "^armv7")
    set(SWIFTLIB_DEFAULT_LINK_FLAGS
        "-Xlinker -segalign -Xlinker 0x4000")
  endif()

  # If we're supposed to build this library for the target...
  if(SWIFTLIB_TARGET_LIBRARY AND SWIFT_SDKS)
    # Check whether we should build the default variant.
    # FIXME: This is a hack. OSX should just be another SDK.
    if (SWIFTLIB_TARGET_SDKS)
      set(SHOULD_CREATE_VARIANT NO)
      foreach(TARGET_SDK ${SWIFTLIB_TARGET_SDKS})
        if (TARGET_SDK STREQUAL SWIFT_DEFAULT_TARGET_SDK)
          set(SHOULD_CREATE_VARIANT YES)
        endif()
      endforeach()
    else()
      set(SHOULD_CREATE_VARIANT YES)
    endif()
    
    if (SHOULD_CREATE_VARIANT)
      # Pass through to build the default configuration.
      add_swift_library_single(
        ${name}
        ${name}
        ${SWIFTLIB_SOURCES}
        DEPENDS ${SWIFTLIB_DEPENDS}
        FRAMEWORK_DEPENDS ${SWIFTLIB_FRAMEWORK_DEPENDS}
        COMPONENT_DEPENDS ${SWIFTLIB_COMPONENT_DEPENDS}
        C_COMPILE_FLAGS
          "${SWIFTLIB_C_COMPILE_FLAGS} ${SWIFTLIB_DEFAULT_C_COMPILE_FLAGS}"
        SWIFT_COMPILE_FLAGS ${SWIFTLIB_SWIFT_COMPILE_FLAGS}
        LINK_FLAGS "${SWIFTLIB_LINK_FLAGS} ${SWIFTLIB_DEFAULT_LINK_FLAGS}"
        FILE_DEPENDS ${SWIFTLIB_FILE_DEPENDS}
        ${SWIFTLIB_API_NOTES_NON_OVERLAY_FLAG}
        ${SWIFTLIB_INSTALL_FLAG}
        ${SWIFTLIB_IS_STDLIB_CORE_FLAG}
        ${SWIFTLIB_NO_LLVM_COMMON_LIBS_FLAG})
    endif ()

    # Loop through the various SDKs building the other variants of
    # this library.
    foreach(SDK ${SWIFT_SDKS})
      set(THIN_INPUT_TARGETS)

      # For each architecture supported by this SDK
      foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
        # Configure variables for this subdirectory.
        set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
        set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")

        # Determine whether we should build this variant.
        if (SWIFTLIB_TARGET_SDKS)
          set(SHOULD_CREATE_VARIANT NO)
          foreach(TARGET_SDK ${SWIFTLIB_TARGET_SDKS})
            if (TARGET_SDK STREQUAL SDK)
              set(SHOULD_CREATE_VARIANT YES)
            endif()
          endforeach()
        else()
          set(SHOULD_CREATE_VARIANT YES)
        endif()

        if (SHOULD_CREATE_VARIANT)
          # Map dependencies over to the appropriate variants.
          set(depends)
          foreach(DEP ${SWIFTLIB_DEPENDS})
            list(APPEND depends "${DEP}${VARIANT_SUFFIX}")
          endforeach()

          # Add -momit-leaf-frame-pointer on x86.
          set(SWIFTLIB_CURRENT_C_COMPILE_FLAGS)
          if (SWIFT_OPTIMIZED)
            if (ARCH STREQUAL "i386")
              set(SWIFTLIB_CURRENT_C_COMPILE_FLAGS "-momit-leaf-frame-pointer")
            elseif (ARCH STREQUAL "x86_64")
              set(SWIFTLIB_CURRENT_C_COMPILE_FLAGS "-momit-leaf-frame-pointer")
            endif()
          endif()
        
          # Use 16K segment alignment on 32-bit arm.
          # The linker does this by default for iOS 8+ deployment, 
          # but we deploy to iOS 7.
          set(SWIFTLIB_CURRENT_LINK_FLAGS)
          if (ARCH MATCHES "^armv7")
            set(SWIFTLIB_CURRENT_LINK_FLAGS 
                "-Xlinker -segalign -Xlinker 0x4000")
          endif()

          # Add this library variant.
          add_swift_library_single(
            ${VARIANT_NAME}
            ${name}
            ${SWIFTLIB_SOURCES}
            SDK ${SDK}
            ARCHITECTURE ${ARCH}
            DEPENDS ${depends}
            FRAMEWORK_DEPENDS ${SWIFTLIB_FRAMEWORK_DEPENDS}
            COMPONENT_DEPENDS ${SWIFTLIB_COMPONENT_DEPENDS}
            FILE_DEPENDS ${SWIFTLIB_FILE_DEPENDS}
            C_COMPILE_FLAGS
              "${SWIFTLIB_C_COMPILE_FLAGS} ${SWIFTLIB_CURRENT_C_COMPILE_FLAGS}"
            SWIFT_COMPILE_FLAGS ${SWIFTLIB_SWIFT_COMPILE_FLAGS}
            LINK_FLAGS "${SWIFTLIB_LINK_FLAGS} ${SWIFTLIB_CURRENT_LINK_FLAGS}"
            ${SWIFTLIB_API_NOTES_NON_OVERLAY_FLAG}
            ${SWIFTLIB_INSTALL_FLAG}
            ${SWIFTLIB_IS_STDLIB_CORE_FLAG}
            ${SWIFTLIB_NO_LLVM_COMMON_LIBS_FLAG})

          # Add dependencies on the (not-yet-created) custom lipo
          # target.
          foreach(DEP ${SWIFTLIB_DEPENDS})
            add_dependencies(${VARIANT_NAME} 
              "${DEP}-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
          endforeach()

          if (SWIFT_BUILD_STATIC AND SWIFTLIB_INSTALL)
            # Add dependencies on the (not-yet-created) custom lipo
            # target.
            foreach(DEP ${SWIFTLIB_DEPENDS})
              add_dependencies("${VARIANT_NAME}-static" 
                "${DEP}-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-static")
            endforeach()
          endif()

          # Note this thin library.
          list(APPEND THIN_INPUT_TARGETS ${VARIANT_NAME})
        endif()
      endforeach(ARCH)

      # Find the library targets created for this SDK.
      if (THIN_INPUT_TARGETS)
        # Determine the name of the universal library.
        if (SHARED_LIBRARY)
          set(UNIVERSAL_LIBRARY_NAME 
            "${SWIFTLIB_DIR}/${SWIFT_SDK_${SDK}_LIB_SUBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${name}${CMAKE_SHARED_LIBRARY_SUFFIX}")
        else()
          set(UNIVERSAL_LIBRARY_NAME 
            "${SWIFTLIB_DIR}/${SWIFT_SDK_${SDK}_LIB_SUBDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}${name}${CMAKE_STATIC_LIBRARY_SUFFIX}")
        endif()

        add_swift_lipo_target(
          "${name}-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
          ${UNIVERSAL_LIBRARY_NAME}
          ${THIN_INPUT_TARGETS})

        # If we built static variants of the library, create a lipo
        # target for them.
        if (SWIFT_BUILD_STATIC AND SWIFTLIB_INSTALL)
          set(THIN_INPUT_TARGETS_STATIC)
          foreach(TARGET ${THIN_INPUT_TARGETS})
            list(APPEND THIN_INPUT_TARGETS_STATIC "${TARGET}-static")
          endforeach()

          add_swift_lipo_target(
            "${name}-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-static"
            "${SWIFTSTATICLIB_DIR}/${SWIFT_SDK_${SDK}_LIB_SUBDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}${name}${CMAKE_STATIC_LIBRARY_SUFFIX}"
            ${THIN_INPUT_TARGETS_STATIC})
        endif()
      endif()
    endforeach(SDK)      
  else()
    # Pass through to build the default configuration.
    add_swift_library_single(
      ${name}
      ${name}
      ${SWIFTLIB_SOURCES}
      DEPENDS ${SWIFTLIB_DEPENDS}
      FRAMEWORK_DEPENDS ${SWIFTLIB_FRAMEWORK_DEPENDS}
      COMPONENT_DEPENDS ${SWIFTLIB_COMPONENT_DEPENDS}
      FILE_DEPENDS ${SWIFTLIB_FILE_DEPENDS}
      C_COMPILE_FLAGS
        "${SWIFTLIB_C_COMPILE_FLAGS} ${SWIFTLIB_DEFAULT_C_COMPILE_FLAGS}"
      SWIFT_COMPILE_FLAGS ${SWIFTLIB_SWIFT_COMPILE_FLAGS}
      LINK_FLAGS "${SWIFTLIB_LINK_FLAGS} ${SWIFTLIB_DEFAULT_LINK_FLAGS}"
      ${SWIFTLIB_API_NOTES_NON_OVERLAY_FLAG}
      ${SWIFTLIB_INSTALL_FLAG}
      ${SWIFTLIB_IS_STDLIB_CORE_FLAG}
      ${SWIFTLIB_NO_LLVM_COMMON_LIBS_FLAG})
  endif()
endmacro(add_swift_library)

# Add a new Swift executable.
#
# Usage:
#   add_swift_executable(name         # Name of the executable (e.g., swift)
#     [DEPENDS dep1 ...]              # Libraries this executable depends on
#     [COMPONENT_DEPENDS comp1 ...]   # LLVM components this executable 
#                                     # depends on
#     [FILE_DEPENDS target1 ...]      # Additional files this executable
#                                     # depends on
#     [EXCLUDE_FROM_ALL]              # Whether to exclude this executable from
#                                     # the ALL_BUILD target
#     [OPT_FLAGS]                     # Optimization flags (overrides SWIFT_OPTIMIZE)
#     [DONT_STRIP_NON_MAIN_SYMBOLS]   # Should we not strip non main symbols. 
#     [DISABLE_ASLR]                  # Should we compile with -Wl,-no_pie so ASLR is disabled?
#     source1 [source2 source3 ...])  # Sources to add into this library
macro(add_swift_executable name)
  # Parse the arguments we were given.
  parse_arguments(SWIFTEXE  "DEPENDS;COMPONENT_DEPENDS;OPT_FLAGS" "EXCLUDE_FROM_ALL;DONT_STRIP_NON_MAIN_SYMBOLS;DISABLE_ASLR"
    ${ARGN})
  set(SWIFTEXE)
  
  handle_swift_sources(SWIFTEXE_DEFAULT_ARGS SWIFTEXE_EXTERNAL_SOURCES ${name}
    DEPENDS ${SWIFTEXE_FILE_DEPENDS} OPT_FLAGS ${SWIFTEXE_OPT_FLAGS}
    IS_MAIN)

  # Add the executable
  if (${SWIFTEXE_EXCLUDE_FROM_ALL})
    add_executable(${name} EXCLUDE_FROM_ALL ${SWIFTEXE_DEFAULT_ARGS} ${SWIFTEXE_EXTERNAL_SOURCES})
  else()
    add_executable(${name} ${SWIFTEXE_DEFAULT_ARGS} ${SWIFTEXE_EXTERNAL_SOURCES})
  endif()
  llvm_update_compile_flags(${name})
  set_source_files_properties(
    ${SWIFTEXE_DEFAULT_ARGS}
    PROPERTIES OBJECT_DEPENDS "${SWIFTEXE_FILE_DEPENDS}")
  set_output_directory(${name} ${SWIFT_RUNTIME_OUTPUT_INTDIR} ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  # Add Swift source files to the project.
  source_group("Swift Sources" FILES ${SWIFTEXE_EXTERNAL_SOURCES})
  # ...and for now, mark them as HEADER_FILE_ONLY, so that Xcode doesn't try
  # to build them itself.
  set_source_files_properties(${SWIFTEXE_EXTERNAL_SOURCES} PROPERTIES
    HEADER_FILE_ONLY true)

  # Add appropriate dependencies
  if( LLVM_COMMON_DEPENDS )
    add_dependencies( ${name} ${LLVM_COMMON_DEPENDS} )
  endif( LLVM_COMMON_DEPENDS )

  set_property(TARGET ${name} APPEND_STRING PROPERTY
    LINK_FLAGS " -L${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR}")
  target_link_libraries( ${name} ${SWIFTEXE_DEPENDS} )
  llvm_config( ${name} ${SWIFTEXE_COMPONENT_DEPENDS} )
  target_link_libraries( ${name} ${LLVM_COMMON_LIBS} )

  if (SWIFTEXE_DISABLE_ASLR)
    set_property(TARGET ${name} APPEND_STRING PROPERTY
      LINK_FLAGS    " -Wl,-no_pie ")
  endif()

  if (SWIFT_SDKS)
    set_property(TARGET ${name} APPEND_STRING PROPERTY
      COMPILE_FLAGS " ${SWIFT_GLOBAL_DEPLOYMENT_TARGET_FLAGS}")
    set_property(TARGET ${name} APPEND_STRING PROPERTY
      LINK_FLAGS    " ${SWIFT_GLOBAL_DEPLOYMENT_TARGET_FLAGS}")
  endif()

  set_target_properties(${name} PROPERTIES FOLDER "Swift executables")

  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set_property(TARGET ${name} APPEND_STRING PROPERTY
      LINK_FLAGS " -Xlinker -rpath -Xlinker @executable_path/../lib/swift/${SWIFTLIB_SUBDIR}")
  endif()
 
  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    # ASan does not play well with exported_symbol option. This should be fixed soon.
    if(NOT SWIFT_ASAN_BUILD)
      if (NOT SWIFTEXE_DONT_STRIP_NON_MAIN_SYMBOLS)
        set_property(TARGET ${name} APPEND_STRING PROPERTY
          LINK_FLAGS " -Xlinker -exported_symbol -Xlinker _main")
      endif()
    endif()
  endif()
endmacro(add_swift_executable)

macro(manpage PODFILE DESC MANFILE SECTION)
  find_program(POD2MAN pod2man)

  if(POD2MAN)
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${MANFILE}.${SECTION}
      DEPENDS ${PODFILE}
      COMMAND ${POD2MAN}
        ARGS --section ${SECTION} --center ${DESC} --release --stderr --name ${MANFILE}
        ${PODFILE} > ${CMAKE_CURRENT_BINARY_DIR}/${MANFILE}.${SECTION}
    )
  
    set(MANPAGE_TARGET "man-${MANFILE}")
  
    add_custom_target(${MANPAGE_TARGET} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${MANFILE}.${SECTION})
  
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${MANFILE}.${SECTION}
            DESTINATION share/man/man${SECTION} )
  else()
    message(WARNING "Need pod2man installed to generate man page")
  endif()
endmacro(manpage)

# Variable that tracks the set of configured SDKs.
#
# Each element in this list is an SDK for which the various
# SWIFT_SDK_${NAME}_* variables are defined. Swift libraries will be
# built for each variant.
set(SWIFT_SDKS)

# Configure an SDK
#
# Usage:
#   configure_sdk(
#     prefix             # Prefix to use for SDK variables (e.g., OSX)
#     name               # Display name for this SDK
#     deployment_version # Deployment version
#     xcrun_name         # SDK name to use with xcrun
#     version_min_name   # The name used in the -mOS-version-min flag
#     triple_name        # The name used in Swift's -triple
#     architectures      # A list of architectures this SDK supports
#     internal           # Whether the prefer the internal SDK, if present
#   )
#     
# This macro attempts to configure a given SDK. When successful, it
# defines a number of variables:
#
#   SWIFT_SDK_${PREFIX}_PATH                Path to the SDK
#   SWIFT_SDK_${PREFIX}_VERSION             SDK version number (e.g., 10.9, 7.0)
#   SWIFT_SDK_${PREFIX}_DEPLOYMENT_VERSION  Deployment version (e.g., 10.9, 7.0)
#   SWIFT_SDK_${PREFIX}_LIB_SUBDIR          Library subdir for this SDK
#   SWIFT_SDK_${PREFIX}_VERSION_MIN_NAME    Version min name for this SDK
#   SWIFT_SDK_${PREFIX}_TRIPLE_NAME         Triple name for this SDK
#   SWIFT_SDK_${PREFIX}_ARCHITECTURES       Architectures (as a list)
macro(configure_sdk PREFIX NAME DEPLOYMENT_VERSION XCRUN_NAME
                    VERSION_MIN_NAME TRIPLE_NAME ARCHITECTURES INTERNAL)
  # Find the SDK
  set(SWIFT_SDK_${PREFIX}_PATH "" CACHE PATH "Path to the ${NAME} SDK")

  if (NOT SWIFT_SDK_${PREFIX}_PATH)
    if (${INTERNAL})
      execute_process(
        COMMAND xcrun -sdk "${XCRUN_NAME}.internal" --show-sdk-path
        OUTPUT_VARIABLE SWIFT_SDK_${PREFIX}_PATH
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    endif()

    if (NOT SWIFT_SDK_${PREFIX}_PATH)
      execute_process(
        COMMAND xcrun -sdk ${XCRUN_NAME} --show-sdk-path
        OUTPUT_VARIABLE SWIFT_SDK_${PREFIX}_PATH
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    endif()
  endif()

  if(EXISTS "${SWIFT_SDK_${PREFIX}_PATH}/System/Library/Frameworks/module.map")
    # Determine the SDK version we found.
    string(REGEX MATCH "/[A-Za-z]+[0-9]+\\.[0-9]+(\\.Internal)?\\.sdk"
           SCRATCH ${SWIFT_SDK_${PREFIX}_PATH})
    string(REGEX MATCH "[0-9]+\\.[0-9]+"
           SWIFT_SDK_${PREFIX}_VERSION ${SCRATCH})
         
    # Set other variables.
    set(SWIFT_SDK_${PREFIX}_NAME ${NAME})
    set(SWIFT_SDK_${PREFIX}_DEPLOYMENT_VERSION ${DEPLOYMENT_VERSION})
    set(SWIFT_SDK_${PREFIX}_LIB_SUBDIR ${XCRUN_NAME})
    set(SWIFT_SDK_${PREFIX}_VERSION_MIN_NAME ${VERSION_MIN_NAME})
    set(SWIFT_SDK_${PREFIX}_TRIPLE_NAME ${TRIPLE_NAME})
    set(SWIFT_SDK_${PREFIX}_ARCHITECTURES ${ARCHITECTURES})

    # Report this SDK to the user.
    message(STATUS "${SWIFT_SDK_${PREFIX}_NAME} SDK:")
    message(STATUS "  Path: ${SWIFT_SDK_${PREFIX}_PATH}")
    message(STATUS "  Version: ${SWIFT_SDK_${PREFIX}_VERSION}")
    message(STATUS "  Deployment version: ${SWIFT_SDK_${PREFIX}_DEPLOYMENT_VERSION}")
    message(STATUS "  Library subdir: ${SWIFT_SDK_${PREFIX}_LIB_SUBDIR}")
    message(STATUS "  Version min name: ${SWIFT_SDK_${PREFIX}_VERSION_MIN_NAME}")
    message(STATUS "  Triple name: ${SWIFT_SDK_${PREFIX}_TRIPLE_NAME}")
    message(STATUS "  Architectures: ${SWIFT_SDK_${PREFIX}_ARCHITECTURES}")

    # Add this to the list of known SDKs.
    list(APPEND SWIFT_SDKS ${PREFIX})
  else()
    message(FATAL_ERROR "${NAME} SDK not found at SWIFT_SDK_${PREFIX}_PATH.")
  endif()
endmacro(configure_sdk)

# Configure the various SDKs.

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  # Choose an SDK if none was set.
  set(MODULES_SDK "" CACHE PATH
      "Path to the modules-enabled SDK to use")
  if (NOT MODULES_SDK)
    execute_process(COMMAND xcrun --sdk macosx --show-sdk-path
      OUTPUT_VARIABLE MODULES_SDK
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()
  if(NOT EXISTS "${MODULES_SDK}/System/Library/Frameworks/module.map")
    message(FATAL_ERROR "A modules-enabled SDK is required for Swift on OS X.")
  endif()

  # Choose a deployment target if none was set.
  set(SWIFT_DEPLOYMENT_TARGET "" CACHE STRING 
      "Deployment target for Swift compiler and libraries.")
  if (NOT SWIFT_DEPLOYMENT_TARGET)
    execute_process(COMMAND sw_vers -productVersion
                    OUTPUT_VARIABLE SWIFT_DEPLOYMENT_TARGET
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REGEX MATCH "[0-9]+\\.[0-9]+" SWIFT_DEPLOYMENT_TARGET ${SWIFT_DEPLOYMENT_TARGET})
  endif()

  # Choose a deployment OS if none was set.
  set(SWIFT_DEPLOYMENT_OS "macosx" CACHE STRING
      "Deployment OS for Swift compiler and libraries [macosx, iphoneos, iphonesimulator].")

  # Sadly there are three OS naming conventions.
  # xcrun SDK name:   macosx iphoneos iphonesimulator (+ "internal" or version)
  # -mOS-version-min: macosx ios      ios-simulator
  # swift -triple:    macosx ios      ios

  if (SWIFT_DEPLOYMENT_OS MATCHES "^iphoneos")
    set(version_min_os "ios")
    set(triple_os "ios")
    set(SWIFT_DEFAULT_TARGET_SDK "IOS")
  elseif (SWIFT_DEPLOYMENT_OS MATCHES "^iphonesimulator")
    set(version_min_os "ios-simulator")
    set(triple_os "ios")
    set(SWIFT_DEFAULT_TARGET_SDK "IOS_SIMULATOR")
  elseif (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
    set(version_min_os "macosx")
    set(triple_os "macosx")
    set(SWIFT_DEFAULT_TARGET_SDK "OSX")
  else()
    message(FATAL_ERROR "Can't understand SWIFT_DEPLOYMENT_OS '${SWIFT_DEPLOYMENT_OS}';")
  endif()

  # Add host triple to swift compiler flags.
  set(SWIFT_HOST_TRIPLE "${CMAKE_SYSTEM_PROCESSOR}-apple-${triple_os}${SWIFT_DEPLOYMENT_TARGET}" CACHE STRING "triple")

  # Determine deployment flags for the C/C++ compiler and linker.
  set(SWIFT_GLOBAL_DEPLOYMENT_TARGET_FLAGS
    "-m${version_min_os}-version-min=${SWIFT_DEPLOYMENT_TARGET}")

  # Add OS name to stdlib install directory
  set(SWIFTLIB_SUBDIR "${SWIFT_DEPLOYMENT_OS}")

  # Configure the requested SDKs.
  # FIXME: Once iOS builds are working this way, add OS X as "just
  # another SDK".
  #   - iOS
  option(SWIFT_BUILD_IOS "Build for iOS" NO)
  if (SWIFT_BUILD_IOS)
    configure_sdk(IOS "iOS" "7.0" iphoneos ios ios "armv7;arm64" YES)
  endif()

  #   - iOS simulator
  option(SWIFT_BUILD_IOS_SIMULATOR "Build for iOS Simulator" NO)
  if (SWIFT_BUILD_IOS_SIMULATOR)
    configure_sdk(IOS_SIMULATOR "iOS Simulator" "7.0" iphonesimulator 
                  ios-simulator ios "i386;x86_64" NO)
  endif()

  if (SWIFT_SDKS)
    message(STATUS "Swift building for SDKs: ${SWIFT_SDKS}")
  endif()

  if (NOT SWIFT_SDKS)
    # Add deployment target to C/C++ compiler and linker flags.
    # FIXME: CMAKE_OSX_DEPLOYMENT_TARGET falls over when used for iOS versions.
    if (XCODE)
      if (${SWIFT_DEPLOYMENT_OS} MATCHES "^macosx") 
        set(CMAKE_XCODE_ATTRIBUTE_MACOSX_DEPLOYMENT_TARGET ${SWIFT_DEPLOYMENT_TARGET})
      else()
        set(CMAKE_XCODE_ATTRIBUTE_IPHONEOS_DEPLOYMENT_TARGET ${SWIFT_DEPLOYMENT_TARGET})
      endif()
    else()
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m${version_min_os}-version-min=${SWIFT_DEPLOYMENT_TARGET}")
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m${version_min_os}-version-min=${SWIFT_DEPLOYMENT_TARGET}")
      set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -m${version_min_os}-version-min=${SWIFT_DEPLOYMENT_TARGET}")
      set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -m${version_min_os}-version-min=${SWIFT_DEPLOYMENT_TARGET}")
    endif()
  endif()
else()
  # Default to the host triple llvm uses for all other platforms
  set(SWIFT_HOST_TRIPLE "${TARGET_TRIPLE}" CACHE STRING "triple")
endif()

# We'll need this once we have generated headers
include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  )

file(GLOB SWIFT_API_NOTES_INPUT_FILES "${SWIFT_API_NOTES_PATH}/*.apinotes")
foreach(file ${SWIFT_API_NOTES_INPUT_FILES})
  get_filename_component(name "${file}" NAME_WE)
  list(APPEND SWIFT_API_NOTES_INPUTS ${name})
endforeach()

if(EXISTS "${SWIFT_API_NOTES_PATH}/.git/HEAD")
  # Trigger updates if the apinotes repository version changes.
  configure_file(
    "${SWIFT_API_NOTES_PATH}/.git/HEAD"
    "${CMAKE_BINARY_DIR}/apinotes-version.check_cache"
    @ONLY)
endif()

# Add all of the subdirectories, where we actually do work.
if (SWIFT_BUILD_TOOLS)
  add_subdirectory(include)
  add_subdirectory(lib)
  add_subdirectory(tools)
  add_subdirectory(utils)
endif (SWIFT_BUILD_TOOLS)
if (SWIFT_BUILD_STDLIB)
  add_subdirectory(stdlib)
endif (SWIFT_BUILD_STDLIB)
  add_subdirectory(examples)

option(SWIFT_INCLUDE_TESTS "Create targets for building/running tests." "${LLVM_INCLUDE_TESTS}")
  add_subdirectory(benchmark)
endif()
if (SWIFT_INCLUDE_TESTS)
  add_subdirectory(test)
  add_subdirectory(unittests)
endif()

option(SWIFT_INCLUDE_DOCS "Create targets for building docs." "${LLVM_INCLUDE_DOCS}")
if (SWIFT_INCLUDE_DOCS)
  add_subdirectory(docs)
endif()

# Add a documentation target so that documentation shows up in the
# Xcode project.
if(XCODE)
  add_custom_target(Documentation
    SOURCES README.txt
    docs/LangRef.html
    docs/toc.js)

  file(GLOB SWIFT_TOPLEVEL_HEADERS
    ${CMAKE_CURRENT_SOURCE_DIR}/include/swift${dir}/*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/swift${dir}/*.td
    ${CMAKE_CURRENT_SOURCE_DIR}/include/swift${dir}/*.def)
  add_custom_target(Miscellaneous
    SOURCES ${SWIFT_TOPLEVEL_HEADERS})
endif()

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
 
