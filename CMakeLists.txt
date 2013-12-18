cmake_minimum_required(VERSION 2.8)

# Support building Swift as a standalone project, using LLVM as an
# external library.
if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
  project(Swift)

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

  if (EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/llvm-config${CMAKE_EXECUTABLE_SUFFIX}")
    set (PATH_TO_LLVM_CONFIG "${SWIFT_PATH_TO_LLVM_BUILD}/bin/llvm-config${CMAKE_EXECUTABLE_SUFFIX}")
  elseif (EXISTS "${SWIFT_PATH_TO_LLVM_BUILD}/bin/Debug/llvm-config${CMAKE_EXECUTABLE_SUFFIX}")
    # Looking for bin/Debug/llvm-config is a complete hack. How can we get
    # around this?
    set (PATH_TO_LLVM_CONFIG "${SWIFT_PATH_TO_LLVM_BUILD}/bin/Debug/llvm-config${CMAKE_EXECUTABLE_SUFFIX}")
  else()
    message(FATAL_ERROR "Please set SWIFT_PATH_TO_LLVM_BUILD to a directory containing a LLVM build.")
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

  # MSVC has a gazillion warnings with this.
  if( MSVC )
    set(LLVM_ENABLE_WARNINGS "Enable compiler warnings." OFF)
  else( MSVC )
    set(LLVM_ENABLE_WARNINGS "Enable compiler warnings." ON)
  endif()

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

  exec_program("${PATH_TO_LLVM_CONFIG} --bindir" OUTPUT_VARIABLE LLVM_BINARY_DIR)
  set(LLVM_TABLEGEN_EXE "${LLVM_BINARY_DIR}/llvm-tblgen${CMAKE_EXECUTABLE_SUFFIX}")

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

set(LLVM_RUNTIME_OUTPUT_INTDIR ${PATH_TO_LLVM_BUILD}/bin/${CMAKE_CFG_INTDIR})
set(LLVM_LIBRARY_OUTPUT_INTDIR ${PATH_TO_LLVM_BUILD}/lib/${CMAKE_CFG_INTDIR})

set(SWIFT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SWIFT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

option(SWIFT_OPTIMIZED "Enable optimization for Swift source files" YES)
option(SWIFT_ASSERTS "Enable assertions for the Swift standard library" YES)
set(SWIFT_MODULE_CACHE_PATH "" CACHE PATH
  "Directory to use as the Clang module cache when building Swift source files")

option(SWIFT_BUILD_TOOLS "Build Swift tools" YES)
option(SWIFT_BUILD_STDLIB "Build Swift stdlib" YES)
set(SWIFT_RUN_LONG_TESTS OFF CACHE BOOL "Run tests that can take a long time to finish")

set(SWIFT_EXTRA_FLAGS "" CACHE STRING
  "Extra flags to pass when compiling swift files")

set(SWIFT_COMPILER "swift" CACHE FILEPATH "Path to Swift compiler")

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
  # Enable some warnings not enabled by default.
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_ABOUT_RETURN_TYPE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_ABOUT_MISSING_NEWLINE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_VALUE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_VARIABLE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_SIGN_COMPARE "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNUSED_FUNCTION "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_INITIALIZER_NOT_FULLY_BRACKETED "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_HIDDEN_VIRTUAL_FUNCTIONS "YES")
  set(CMAKE_XCODE_ATTRIBUTE_GCC_WARN_UNINITIALIZED_AUTOS "YES")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_WARN_DOCUMENTATION_COMMENTS "YES")

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

# Compile with -Wdocumentation.
check_cxx_compiler_flag("-Werror -Wdocumentation" CXX_SUPPORTS_DOCUMENTATION_FLAG)
if( CXX_SUPPORTS_DOCUMENTATION_FLAG )
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wdocumentation" )
endif()

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
#     [IS_MAIN]                         # This is an executable, not a library
#     [IS_STDLIB_CORE])                 # This is the core standard library
function(compile_swift_files)
  parse_arguments(SWIFTFILE "OUTPUT;SOURCES;FLAGS" "IS_MAIN;IS_STDLIB_CORE"
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
  if(SWIFT_OPTIMIZED)
    list(APPEND swift_flags "-O2")
  else()
    list(APPEND swift_flags "-O0")
  endif()

  # Allow import of other Swift modules we just built.
  list(APPEND swift_flags "-I=${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR}")

  # Turn on debug symbols in debug builds.
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    list(APPEND swift_flags "-g")
  endif()
  
  # If we have a modules SDK, use it for building.
  if (MODULES_SDK)
    list(APPEND swift_flags "-sdk=${MODULES_SDK}")
  endif()

  # If we have a host triple, use it.
  if (SWIFT_HOST_TRIPLE)
    list(APPEND swift_flags "-triple=${SWIFT_HOST_TRIPLE}")
  endif()

  # If we have a custom module cache path, use it.
  if (SWIFT_MODULE_CACHE_PATH)
    list(APPEND swift_flags "-module-cache-path=${SWIFT_MODULE_CACHE_PATH}")
  endif()

  list(APPEND swift_flags "${SWIFT_EXTRA_FLAGS}")

  # FIXME: Cleaner way to do this?

  if(NOT SWIFTFILE_IS_MAIN)
    list(APPEND swift_flags -parse-as-library)
  endif()
  if(SWIFTFILE_IS_STDLIB_CORE)
    list(APPEND swift_flags -parse-stdlib -module-name swift)
  endif()

  set(objfile ${SWIFTFILE_OUTPUT})
  if (NOT IS_ABSOLUTE ${objfile})
    set(objfile
      "${CMAKE_CURRENT_BINARY_DIR}/${objfile}")
  endif()

  if(SWIFTFILE_IS_MAIN)
    set(module_source_file)
    set(postprocessing)
  else()
    get_filename_component(module_source_name ${objfile} NAME_WE)
    set(module_source_dir ${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR})

    set(module_source_file ${module_source_dir}/${module_source_name}.swiftmodule)
    set(intermediate_module_source_file ${CMAKE_CURRENT_BINARY_DIR}/${module_source_name}.swiftmodule)
    list(APPEND swift_flags -emit-module)
    set(postprocessing COMMAND ${CMAKE_COMMAND} -E rename ${intermediate_module_source_file} ${module_source_file})

    install(FILES ${module_source_file}
      DESTINATION lib${LLVM_LIBDIR_SUFFIX}/${SWIFTLIB_SUBDIR})
  endif()

  set(stdlib_dep)
  # Make sure that standard library does not depend on itself.
  if(NOT SWIFTFILE_IS_STDLIB_CORE)
    set(stdlib_dep swift_stdlib_core)
  endif()

  add_custom_command(
    OUTPUT ${objfile} ${module_source_file}
    COMMAND ${SWIFT_COMPILER} "-c" ${swift_flags} "-o" "${objfile}" ${source_files}
    ${postprocessing} 
    DEPENDS ${SWIFT_COMPILER} ${stdlib_dep} ${source_files}
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
    PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${objfile};${module_source_file}")
endfunction()

# Process the sources within the given variable, pulling out any Swift
# sources to be compiled with 'swift' directly. This updates
# ${sourcesvar} in place with the resulting list and ${externalvar} with the
# list of externally-build sources.
#
# Usage:
#   handle_swift_sources(sourcesvar externalvar target)
function(handle_swift_sources sourcesvar externalvar target)
  parse_arguments(SWIFTSOURCES  "" "IS_MAIN;IS_STDLIB_CORE" ${ARGN})

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

  set(swift_flags)
  set(IS_MAIN_arg)
  if (SWIFTSOURCES_IS_MAIN)
    set(IS_MAIN_arg IS_MAIN)
  else()
    set(swift_flags -module-link-name "${target}")
  endif()
  set(IS_STDLIB_CORE_arg)
  if(SWIFTSOURCES_IS_STDLIB_CORE)
    set(IS_STDLIB_CORE_arg IS_STDLIB_CORE)
  endif()

  if(swift_sources)

    # Special-case hack to create swift.o for the swift_stdlib_* targets
    if(target STREQUAL swift_stdlib_core)
      set(swift_obj_base "swift")
    elseif(target STREQUAL swift_stdlib_posix)
      set(swift_obj_base "POSIX")
    else()
      # Otherwise, get the name from the first swift input file.  Also
      # a hack!
      list(GET swift_sources 0 swift_obj_base)
      get_filename_component(swift_obj_base ${swift_obj_base} NAME_WE)
    endif()

    set(swift_obj "${swift_obj_base}${CMAKE_C_OUTPUT_EXTENSION}")
    compile_swift_files(OUTPUT ${swift_obj}
      SOURCES ${swift_sources}
      FLAGS ${swift_flags}
      ${IS_MAIN_arg} ${IS_STDLIB_CORE_arg})
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

# Set SWIFTLIB_SUBDIR to the subdirectory inside SWIFTLIB_DIR 
# where Swift .swiftmodules and libraries should be placed.
# This is changed later based on SWIFT_DEPLOYMENT_OS.
# The OS is currently needed in lieu of proper SDK support.
set(SWIFTLIB_SUBDIR "swift")


# Add a new Swift library.
#
# Usage:
#   add_swift_library(name           # Name of the library (e.g., swiftParse)
#     [DEPENDS dep1 ...]             # Libraries this library depends on
#     [COMPONENT_DEPENDS comp1 ...]  # LLVM components this library depends on
#     [INSTALL]                      # Install library to lib/swift
#     [IS_STDLIB_CORE]               # Compile as the standard library core
#     source1 [source2 source3 ...]) # Sources to add into this library
macro(add_swift_library name)
  parse_arguments(SWIFTLIB "DEPENDS;COMPONENT_DEPENDS" "INSTALL;IS_STDLIB_CORE"
    ${ARGN})
  set(SWIFTLIB_SOURCES ${SWIFTLIB_DEFAULT_ARGS})

  if(MSVC_IDE OR XCODE)
    string( REGEX MATCHALL "/[^/]+" split_path ${CMAKE_CURRENT_SOURCE_DIR})
    list( GET split_path -1 dir)
    file( GLOB_RECURSE SWIFTLIB_HEADERS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.h
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.def)

    file( GLOB_RECURSE SWIFTLIB_TDS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.td)
    source_group("TableGen descriptions" FILES ${SWIFTLIB_TDS})

    set(SWIFTLIB_SOURCES ${SWIFTLIB_SOURCES} ${SWIFTLIB_HEADERS} ${SWIFTLIB_TDS})
  endif(MSVC_IDE OR XCODE)

  if (MODULE)
    set(libkind MODULE)
  elseif (SHARED_LIBRARY)
    set(libkind SHARED)
  else()
    set(libkind)
  endif()

  set(IS_STDLIB_CORE_arg)
  if(SWIFTLIB_IS_STDLIB_CORE)
    set(IS_STDLIB_CORE_arg IS_STDLIB_CORE)
  endif()
  
  handle_swift_sources(SWIFTLIB_SOURCES SWIFTLIB_EXTERNAL_SOURCES ${name}
    ${IS_STDLIB_CORE_arg})
  add_library( ${name} ${libkind}
    ${SWIFTLIB_SOURCES}
    ${SWIFTLIB_EXTERNAL_SOURCES})

  if( LLVM_COMMON_DEPENDS )
    add_dependencies( ${name} ${LLVM_COMMON_DEPENDS} )
  endif( LLVM_COMMON_DEPENDS )

  # Add Swift source files to the project.
  source_group("Swift Sources" FILES ${SWIFTLIB_EXTERNAL_SOURCES})
  # ...and for now, mark them as HEADER_FILE_ONLY, so that Xcode doesn't try
  # to build them itself.
  set_source_files_properties(${SWIFTLIB_EXTERNAL_SOURCES} PROPERTIES
    HEADER_FILE_ONLY true)

  target_link_libraries( ${name} "-L${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR}" ${SWIFTLIB_DEPENDS} )
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
    # Install runtime libraries to lib/swift instead of lib. This works around
    # the fact that -isysroot prevents linking to libraries in the system
    # /usr/lib if Swift is installed in /usr.
    set_target_properties(${name} PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR}
      ARCHIVE_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR} )

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
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SUBDIR}
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SUBDIR})
    endforeach()

    install(TARGETS ${name}
      LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}/${SWIFTLIB_SUBDIR}
      ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX}/${SWIFTLIB_SUBDIR})

    # FIXME: Install .swift files so they can be parsed on import.
    # Really we want to install a preparsed metadata file.
    foreach(external_source ${SWIFTLIB_EXTERNAL_SOURCES})
      get_filename_component(extension ${external_source} EXT)
      if(extension STREQUAL ".swift")
        set(filename ${external_source})
      endif()
    endforeach(external_source)
  endif(SWIFTLIB_INSTALL)

  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_target_properties(${name}
      PROPERTIES
      INSTALL_NAME_DIR "@rpath"
      BUILD_WITH_INSTALL_RPATH YES)
  endif()

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
    add_executable(${name} EXCLUDE_FROM_ALL ${SWIFTEXE_DEFAULT_ARGS} ${SWIFTEXE_EXTERNAL_SOURCES})
  else()
    add_executable(${name} ${SWIFTEXE_DEFAULT_ARGS} ${SWIFTEXE_EXTERNAL_SOURCES})
  endif()

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

  target_link_libraries( ${name} "-L${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR}" ${SWIFTEXE_DEPENDS} )
  llvm_config( ${name} ${SWIFTEXE_COMPONENT_DEPENDS} )
  target_link_libraries( ${name} ${LLVM_COMMON_LIBS} )
  link_system_libs( ${name} )

  set_target_properties(${name} PROPERTIES FOLDER "Swift executables")  
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_target_properties(${name}
      PROPERTIES
      LINK_FLAGS "-Xlinker -exported_symbol -Xlinker _main    -Xlinker -rpath -Xlinker @executable_path/../lib/${SWIFTLIB_SUBDIR} ")
  endif()
endmacro(add_swift_executable)

# Add compiler flags for a target.
#
# Usage:
#   add_swift_compiler_flags(name # Name of the target
#     flags                     ) # compiler flags string
function(add_swift_compiler_flags name flags)
  set_property(TARGET ${name} APPEND_STRING PROPERTY COMPILE_FLAGS " ${flags}")
endfunction()

# Add compiler flags for a target when SWIFT_OPTIMIZED is enabled.
#
# Usage:
#   add_swift_optimization_flags(name) # Name of the target
macro(add_swift_optimization_flags name)
  if(SWIFT_OPTIMIZED)
    if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
     add_swift_compiler_flags(${name} "-momit-leaf-frame-pointer")
    elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
     add_swift_compiler_flags(${name} "-momit-leaf-frame-pointer")
   endif()
  endif()
endmacro(add_swift_optimization_flags)

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

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
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
  elseif (SWIFT_DEPLOYMENT_OS MATCHES "^iphonesimulator")
    set(version_min_os "ios-simulator")
    set(triple_os "ios")
  elseif (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
    set(version_min_os "macosx")
    set(triple_os "macosx")
  else()
    message(FATAL_ERROR "Can't understand SWIFT_DEPLOYMENT OS '${SWIFT_DEPLOYMENT_OS};")
  endif()

  # Add host triple to swift compiler flags.
  set(SWIFT_HOST_TRIPLE "${CMAKE_SYSTEM_PROCESSOR}-apple-${triple_os}${SWIFT_DEPLOYMENT_TARGET}" CACHE STRING "triple")

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

  # Add OS name to stdlib install directory
  set(SWIFTLIB_SUBDIR "swift/${SWIFT_DEPLOYMENT_OS}")

endif()

# We'll need this once we have generated headers
include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  )

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
