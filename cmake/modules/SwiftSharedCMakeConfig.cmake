include(CMakeParseArguments)

# Invokes llvm_config to get various configuration information needed to compile
# programs which use llvm.
#
# Optional output parameters:
#   [ENABLE_ASSERTIONS enableAssertions]
#     If assertions are enabled.
#
#   [TOOLS_BINARY_DIR toolsBinaryDir]
#     Path to llvm tools bin directory.
#
#   [LIBRARY_DIR toolsLibDir]
#     Path to llvm tools lib directory.
#
#   [INCLUDE_DIR includeDir]
#     Path to llvm include directory.
#
#   [OBJ_ROOT_DIR objRootDir]
#     Path tp llvm build tree.
#
#   [SOURCE_DIR srcDir]
#     Path to llvm source tree.
function(_swift_llvm_config_info)
  # Parse the arguments we were given.
  set(onevalueargs ENABLE_ASSERTIONS TOOLS_BINARY_DIR LIBRARY_DIR INCLUDE_DIR OBJ_ROOT_DIR SOURCE_DIR)
  cmake_parse_arguments(
    SWIFTLLVM
    "" # options
    "${onevalueargs}"
    "" # multi-value args
    ${ARGN})

  find_program(LLVM_CONFIG "llvm-config")
  if(NOT LLVM_CONFIG)
    message(FATAL_ERROR "llvm-config not found -- ${LLVM_CONFIG}")
  endif()

  set(CONFIG_OUTPUT)
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

  list(GET CONFIG_OUTPUT 0 ENABLE_ASSERTIONS)
  list(GET CONFIG_OUTPUT 1 TOOLS_BINARY_DIR)
  list(GET CONFIG_OUTPUT 2 LIBRARY_DIR)
  list(GET CONFIG_OUTPUT 3 INCLUDE_DIR)
  list(GET CONFIG_OUTPUT 4 LLVM_OBJ_ROOT)
  list(GET CONFIG_OUTPUT 5 MAIN_SRC_DIR)

  if(SWIFTLLVM_ENABLE_ASSERTIONS)
    set("${SWIFTLLVM_ENABLE_ASSERTIONS}" "${ENABLE_ASSERTIONS}" PARENT_SCOPE)
  endif()
  if(SWIFTLLVM_TOOLS_BINARY_DIR)
    set("${SWIFTLLVM_TOOLS_BINARY_DIR}" "${TOOLS_BINARY_DIR}" PARENT_SCOPE)
  endif()
  if(SWIFTLLVM_LIBRARY_DIR)
    set("${SWIFTLLVM_LIBRARY_DIR}" "${LIBRARY_DIR}" PARENT_SCOPE)
  endif()
  if(SWIFTLLVM_INCLUDE_DIR)
    set("${SWIFTLLVM_INCLUDE_DIR}" "${INCLUDE_DIR}" PARENT_SCOPE)
  endif()
  if(SWIFTLLVM_OBJ_ROOT_DIR)
    set("${SWIFTLLVM_OBJ_ROOT_DIR}" "${LLVM_OBJ_ROOT}" PARENT_SCOPE)
  endif()
  if(SWIFTLLVM_SOURCE_DIR)
    set("${SWIFTLLVM_SOURCE_DIR}" "${MAIN_SRC_DIR}" PARENT_SCOPE)
  endif()
endfunction()

# Common cmake project config for standalone builds.
#
# Parameters:
#   product
#     The product name, e.g. Swift or SourceKit. Used as prefix for some
#     cmake variables.
#
#   is_cross_compiling
#     Whether this is cross-compiling host tools.
macro(swift_common_standalone_build_config product is_cross_compiling)
  if(${is_cross_compiling})
    # Can't run llvm-config from the cross-compiled LLVM.
    set(LLVM_TOOLS_BINARY_DIR "" CACHE PATH "Path to llvm/bin")
    set(LLVM_LIBRARY_DIR "" CACHE PATH "Path to llvm/lib")
    set(LLVM_MAIN_INCLUDE_DIR "" CACHE PATH "Path to llvm/include")
    set(LLVM_BINARY_DIR "" CACHE PATH "Path to LLVM build tree")
    set(LLVM_MAIN_SRC_DIR "" CACHE PATH "Path to LLVM source tree")
  else()
    # Rely on llvm-config.
    _swift_llvm_config_info(
        ENABLE_ASSERTIONS llvm_config_enable_assertions
        TOOLS_BINARY_DIR llvm_config_tools_binary_dir
        LIBRARY_DIR llvm_config_library_dir
        INCLUDE_DIR llvm_config_include_dir
        OBJ_ROOT_DIR llvm_config_obj_root
        SOURCE_DIR llvm_config_src_dir
    )
    if(NOT MSVC_IDE)
      set(LLVM_ENABLE_ASSERTIONS ${llvm_config_enable_assertions}
        CACHE BOOL "Enable assertions")
      # Assertions should follow llvm-config's.
      mark_as_advanced(LLVM_ENABLE_ASSERTIONS)
    endif()

    set(LLVM_TOOLS_BINARY_DIR "${llvm_config_tools_binary_dir}" CACHE PATH "Path to llvm/bin")
    set(LLVM_LIBRARY_DIR "${llvm_config_library_dir}" CACHE PATH "Path to llvm/lib")
    set(LLVM_MAIN_INCLUDE_DIR "${llvm_config_include_dir}" CACHE PATH "Path to llvm/include")
    set(LLVM_BINARY_DIR "${llvm_config_obj_root}" CACHE PATH "Path to LLVM build tree")
    set(LLVM_MAIN_SRC_DIR "${llvm_config_src_dir}" CACHE PATH "Path to LLVM source tree")

    set(${product}_NATIVE_LLVM_TOOLS_PATH "${LLVM_TOOLS_BINARY_DIR}")
    set(${product}_NATIVE_CLANG_TOOLS_PATH "${LLVM_TOOLS_BINARY_DIR}")
  endif()

  find_program(LLVM_TABLEGEN_EXE "llvm-tblgen" "${${product}_NATIVE_LLVM_TOOLS_PATH}"
    NO_DEFAULT_PATH)

  set(LLVM_CMAKE_PATH "${LLVM_BINARY_DIR}/share/llvm/cmake")
  list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_PATH}")

  set(LLVMCONFIG_FILE "${LLVM_CMAKE_PATH}/LLVMConfig.cmake")
  if(NOT EXISTS ${LLVMCONFIG_FILE})
    message(FATAL_ERROR "Not found: ${LLVMCONFIG_FILE}")
  endif()

  # Save and restore variables that LLVM configuration overrides.
  set(LLVM_TOOLS_BINARY_DIR_saved "${LLVM_TOOLS_BINARY_DIR}")
  set(LLVM_ENABLE_ASSERTIONS_saved "${LLVM_ENABLE_ASSERTIONS}")
  include(${LLVMCONFIG_FILE})
  set(LLVM_TOOLS_BINARY_DIR "${LLVM_TOOLS_BINARY_DIR_saved}")
  set(LLVM_ENABLE_ASSERTIONS "${LLVM_ENABLE_ASSERTIONS_saved}")

  # Clang
  set(${product}_PATH_TO_LLVM_SOURCE "${LLVM_MAIN_SRC_DIR}")
  set(${product}_PATH_TO_LLVM_BUILD "${LLVM_BINARY_DIR}")

  set(PATH_TO_LLVM_SOURCE "${${product}_PATH_TO_LLVM_SOURCE}")
  set(PATH_TO_LLVM_BUILD "${${product}_PATH_TO_LLVM_BUILD}")

  set(${product}_PATH_TO_CLANG_SOURCE "${${product}_PATH_TO_LLVM_SOURCE}/tools/clang"
      CACHE PATH "Path to Clang source code.")
  set(${product}_PATH_TO_CLANG_BUILD "${${product}_PATH_TO_LLVM_BUILD}" CACHE PATH
    "Path to the directory where Clang was built or installed.")

  if( NOT EXISTS "${${product}_PATH_TO_CLANG_SOURCE}/include/clang/AST/Decl.h" )
    message(FATAL_ERROR "Please set ${product}_PATH_TO_CLANG_SOURCE to the root directory of Clang's source code.")
  else()
    get_filename_component(CLANG_MAIN_SRC_DIR ${${product}_PATH_TO_CLANG_SOURCE}
      ABSOLUTE)
  endif()

  if(EXISTS "${${product}_PATH_TO_CLANG_BUILD}/include/clang/Basic/Version.inc")
    set(CLANG_BUILD_INCLUDE_DIR "${${product}_PATH_TO_CLANG_BUILD}/include")
  elseif(EXISTS "${${product}_PATH_TO_CLANG_BUILD}/tools/clang/include/clang/Basic/Version.inc")
    set(CLANG_BUILD_INCLUDE_DIR "${${product}_PATH_TO_CLANG_BUILD}/tools/clang/include")
  else()
    message(FATAL_ERROR "Please set ${product}_PATH_TO_CLANG_BUILD to a directory containing a Clang build.")
  endif()
  if(CLANG_MAIN_INCLUDE_DIR)
    get_filename_component(CLANG_MAIN_SRC_DIR ${${product}_PATH_TO_CLANG_SOURCE}
      ABSOLUTE)
  endif()

  list(APPEND CMAKE_MODULE_PATH "${${product}_PATH_TO_LLVM_BUILD}/share/llvm/cmake")

  get_filename_component(PATH_TO_LLVM_BUILD "${${product}_PATH_TO_LLVM_BUILD}"
    ABSOLUTE)
  get_filename_component(PATH_TO_CLANG_BUILD "${${product}_PATH_TO_CLANG_BUILD}"
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
  link_directories(
      "${LLVM_LIBRARY_DIR}"
      # FIXME: if we want to support separate Clang builds and mix different
      # build configurations of Clang and Swift, this line should be adjusted.
      "${PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/lib")

  set(LIT_ARGS_DEFAULT "-sv")
  if(MSVC OR XCODE)
    set(LIT_ARGS_DEFAULT "${LIT_ARGS_DEFAULT} --no-progress-bar")
  endif()
  set(LLVM_LIT_ARGS "${LIT_ARGS_DEFAULT}" CACHE STRING "Default options for lit")

  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")

  set(LLVM_INCLUDE_TESTS TRUE)
  set(LLVM_INCLUDE_DOCS TRUE)
endmacro()

# Common cmake project config for unified builds.
#
# Parameters:
#   product
#     The product name, e.g. Swift or SourceKit. Used as prefix for some
#     cmake variables.
macro(swift_common_unified_build_config product)
  set(PATH_TO_LLVM_SOURCE "${CMAKE_SOURCE_DIR}")
  set(PATH_TO_LLVM_BUILD "${CMAKE_BINARY_DIR}")
  set(${product}_PATH_TO_CLANG_BUILD "${CMAKE_BINARY_DIR}")
  set(PATH_TO_CLANG_BUILD "${CMAKE_BINARY_DIR}")
  set(CLANG_MAIN_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/tools/clang/include")
  set(CLANG_BUILD_INCLUDE_DIR "${CMAKE_BINARY_DIR}/tools/clang/include")
  set(${product}_NATIVE_LLVM_TOOLS_PATH "${CMAKE_BINARY_DIR}/bin")
  set(${product}_NATIVE_CLANG_TOOLS_PATH "${CMAKE_BINARY_DIR}/bin")

  include_directories(
      "${CLANG_BUILD_INCLUDE_DIR}"
      "${CLANG_MAIN_INCLUDE_DIR}")

  check_cxx_compiler_flag("-Werror -Wnested-anon-types" CXX_SUPPORTS_NO_NESTED_ANON_TYPES_FLAG)
  if( CXX_SUPPORTS_NO_NESTED_ANON_TYPES_FLAG )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-nested-anon-types")
  endif()
endmacro()

# Common additional cmake project config for Xcode.
#
macro(swift_common_xcode_cxx_config)
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
endmacro()

# Common cmake project config for additional warnings.
#
macro(swift_common_cxx_warnings)
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
endmacro()
