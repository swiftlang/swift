if(NOT SWIFT_BUILD_SWIFT_SYNTAX)
  return()
endif()
if(NOT EXISTS "${SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE}")
  message(SEND_ERROR "swift-syntax is required to build the Swift compiler. Please run update-checkout or specify SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE")
  return()
endif()

# Build swift-syntax libraries with FetchContent.
set(CMAKE_SYSTEM_PROCESSOR ${SWIFT_HOST_VARIANT_ARCH})
set(CMAKE_OSX_ARCHITECTURES ${SWIFT_HOST_VARIANT_ARCH})
set(CMAKE_C_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
set(CMAKE_CXX_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
set(CMAKE_Swift_COMPILER_TARGET ${SWIFT_HOST_TRIPLE})
set(BUILD_SHARED_LIBS ON)
# All libraries in 'swift-syntax' goes to 'lib/swift/host'.
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${SWIFT_HOST_LIBRARIES_DEST_DIR}")
if(SWIFT_HOST_VARIANT_SDK MATCHES "LINUX|ANDROID|OPENBSD|FREEBSD")
  set(SWIFT_HOST_LIBRARIES_RPATH "$ORIGIN;$ORIGIN/../${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}")
endif()
# Add unique ABI prefix to swift-syntax libraries so that compiler libraries (e.g. sourcekitdInProc)
# can be used from tools that has its own swift-syntax libraries as SwiftPM dependencies.
set(SWIFT_MODULE_ABI_NAME_PREFIX "Compiler")
set(SWIFTSYNTAX_PACKAGE_NAME "Toolchain")
set(SWIFTSYNTAX_EMIT_MODULE ON)

file(TO_CMAKE_PATH "${SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE}" swift_syntax_path)
FetchContent_Declare(SwiftSyntax SOURCE_DIR "${swift_syntax_path}")
FetchContent_MakeAvailable(SwiftSyntax)

# FIXME: Use FetchContent_Declare's EXCLUDE_FROM_ALL after CMake 3.28
FetchContent_GetProperties(SwiftSyntax BINARY_DIR binary_dir)
set_property(DIRECTORY "${binary_dir}" PROPERTY EXCLUDE_FROM_ALL TRUE)

# Install swift-syntax libraries.
set(SWIFT_SYNTAX_MODULES
  SwiftBasicFormat
  SwiftIfConfig
  SwiftParser
  SwiftParserDiagnostics
  SwiftDiagnostics
  SwiftSyntax
  SwiftOperators
  SwiftSyntaxBuilder
  SwiftSyntaxMacros
  SwiftSyntaxMacroExpansion
  SwiftCompilerPluginMessageHandling
  # Support for LSP
  SwiftIDEUtils
  SwiftRefactor
  # For swift-plugin-server
  SwiftLibraryPluginProvider
)

# Install shared runtime libraries
if(CMAKE_SYSTEM_NAME MATCHES Windows)
  swift_install_in_component(TARGETS ${SWIFT_SYNTAX_MODULES}
                             RUNTIME
                             DESTINATION "bin"
                             COMPONENT swift-syntax-lib)
else()
  swift_install_in_component(TARGETS ${SWIFT_SYNTAX_MODULES}
                             LIBRARY
                             DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host"
                             COMPONENT swift-syntax-lib)
endif()

# Install import libraries in Windows.
if(CMAKE_SYSTEM_NAME MATCHES Windows)
  list(TRANSFORM SWIFT_SYNTAX_MODULES
       PREPEND "${CMAKE_IMPORT_LIBRARY_PREFIX}"
       OUTPUT_VARIABLE SWIFT_SYNTAX_IMPORT_LIBRARIES)
  list(TRANSFORM SWIFT_SYNTAX_IMPORT_LIBRARIES
       APPEND "${CMAKE_IMPORT_LIBRARY_SUFFIX}"
       OUTPUT_VARIABLE SWIFT_SYNTAX_IMPORT_LIBRARIES)
  list(TRANSFORM SWIFT_SYNTAX_IMPORT_LIBRARIES
       PREPEND "${SWIFT_HOST_LIBRARIES_DEST_DIR}/"
       OUTPUT_VARIABLE SWIFT_SYNTAX_IMPORT_LIBRARIES)
  swift_install_in_component(PROGRAMS "${SWIFT_SYNTAX_IMPORT_LIBRARIES}"
                             DESTINATION lib
                             COMPONENT swift-syntax-lib)
endif()

add_dependencies(swift-syntax-lib
  ${SWIFT_SYNTAX_MODULES})

if(BOOTSTRAPPING_MODE STREQUAL "CROSSCOMPILE")
    add_dependencies(swift-syntax-lib swift-stdlib-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH})
endif()

# Install Swift module interface files.
foreach(module ${SWIFT_SYNTAX_MODULES})
  set(module_dir "${module}.swiftmodule")
  set(module_file "${SWIFT_HOST_LIBRARIES_DEST_DIR}/${module_dir}/${SWIFT_HOST_MODULE_TRIPLE}")
  swift_install_in_component(FILES "${module_file}.swiftinterface" "${module_file}.private.swiftinterface"
                             DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/${module_dir}"
                             COMPONENT swift-syntax-lib)
endforeach()

export(TARGETS ${SWIFT_SYNTAX_MODULES}
       NAMESPACE SwiftSyntax::
       FILE ${CMAKE_BINARY_DIR}/cmake/modules/SwiftSyntaxConfig.cmake
       EXPORT_LINK_INTERFACE_LIBRARIES)
