# Add a new library that is intended to be linked against by the compiler.
#
# Usage:
#   add_pure_swift_host_library(name
#     source1 [source2 source3 ...]
#     DEPENDENCIES library1 [library2 ...]
#     SWIFT_DEPENDENCIES library1 [library2 ...]
#   )
#
# name
#   Name of the library (e.g., swiftASTGen).
#
# DEPENDENCIES
#   Target names to pass target_link_library
#
# SWIFT_DEPENDENCIES
#   Target names to pass force_target_link_library.
#   TODO: Remove this and use DEPENDENCIES when CMake is fixed
#
# source1 ...
#   Sources to add into this library.
function(add_compiler_swift_library name)
  set(options
        EMIT_MODULE)
  set(single_parameter_options)
  set(multiple_parameter_options
        DEPENDENCIES
        SWIFT_DEPENDENCIES)

  cmake_parse_arguments(ACSL
                        "${options}"
                        "${single_parameter_options}"
                        "${multiple_parameter_options}"
                        ${ARGN})
  set(ACSL_SOURCES ${ACSL_UNPARSED_ARGUMENTS})

  if (ACSL_EMIT_MODULE)
    set(EMIT_MODULE_PARAM "EMIT_MODULE")
  else()
    set(EMIT_MODULE_PARAM "")
  endif()
  
  add_pure_swift_host_library(
    ${name} STATIC ${EMIT_MODULE_PARAM}
    ${ACSL_SOURCES}
    DEPENDENCIES
      ${ACSL_DEPENDENCIES}
    SWIFT_DEPENDENCIES
      ${ACSL_SWIFT_DEPENDENCIES}
  )

  set(c_include_paths
    # LLVM modules and headers.
    "${LLVM_MAIN_INCLUDE_DIR}"
    # Generated LLVM headers.
    "${LLVM_INCLUDE_DIR}"
    # Clang modules and headers.
    ${CLANG_INCLUDE_DIRS}
    # Bridging modules and headers.
    "${SWIFT_MAIN_INCLUDE_DIR}"
    # Generated C headers.
    "${CMAKE_CURRENT_BINARY_DIR}/../../include")
  set(c_include_paths_args)
  foreach(c_include_path ${c_include_paths})
    list(APPEND c_include_paths_args "SHELL: -Xcc -I -Xcc ${c_include_path}")
  endforeach()

  # Prior to 5.9, we have to use the experimental flag for C++ interop.
  if (CMAKE_Swift_COMPILER_VERSION VERSION_LESS 5.9)
    set(cxx_interop_flag "-enable-experimental-cxx-interop")
  else()
    set(cxx_interop_flag "-cxx-interoperability-mode=default")
  endif()

  set(compile_options
    ${c_include_paths_args}
    "SHELL: ${cxx_interop_flag}"
    "SHELL: -Xcc -std=c++17 -Xcc -DCOMPILED_WITH_SWIFT"

    # Necessary to avoid treating IBOutlet and IBAction as keywords
    "SHELL:-Xcc -UIBOutlet -Xcc -UIBAction -Xcc -UIBInspectable"
  )

  target_compile_options(${name} PRIVATE ${compile_options})
endfunction()

add_subdirectory(Sources/LLVMJSON)
add_subdirectory(Sources/ASTGen)
