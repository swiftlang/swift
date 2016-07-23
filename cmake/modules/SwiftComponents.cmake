# Set of Defined Components
# -------------------------
#
# The set of "defined" swift components are as follows:
#
# * autolink-driver -- the Swift driver support tools
# * compiler -- the Swift compiler and (on supported platforms) the REPL.
# * clang-builtin-headers -- install a copy of Clang builtin headers under
#   'lib/swift/clang'.  This is useful when Swift compiler is installed in
#   isolation.
# * clang-resource-dir-symlink -- install a symlink to the Clang resource
#   directory (which contains builtin headers) under 'lib/swift/clang'.  This is
#   useful when Clang and Swift are installed side-by-side.
# * stdlib -- the Swift standard library.
# * stdlib-experimental -- the Swift standard library module for experimental
#   APIs.
# * sdk-overlay -- the Swift SDK overlay.
# * editor-integration -- scripts for Swift integration in IDEs other than
#   Xcode;
# * tools -- tools (other than the compiler) useful for developers writing
#   Swift code.
# * testsuite-tools -- extra tools required to run the Swift testsuite.
# * toolchain-dev-tools -- install development tools useful in a shared toolchain
# * dev -- headers and libraries required to use Swift compiler as a library.
set(_SWIFT_DEFINED_COMPONENTS
  "autolink-driver;compiler;clang-builtin-headers;clang-resource-dir-symlink;clang-builtin-headers-in-clang-resource-dir;stdlib;stdlib-experimental;sdk-overlay;editor-integration;tools;testsuite-tools;toolchain-dev-tools;dev;license;sourcekit-xpc-service;sourcekit-inproc;swift-remote-mirror;swift-remote-mirror-headers")

macro(swift_configure_components)
  # Set the SWIFT_INSTALL_COMPONENTS variable to the default value if it is not passed in via -D
  set(SWIFT_INSTALL_COMPONENTS "${_SWIFT_DEFINED_COMPONENTS}" CACHE STRING
    "A semicolon-separated list of components to install ${_SWIFT_DEFINED_COMPONENTS}")
endmacro()

function(swift_is_installing_component component result_var_name)
  precondition(component MESSAGE "Component name is required")

  if("${component}" STREQUAL "never_install")
    set("${result_var_name}" FALSE PARENT_SCOPE)
  else()
    list(FIND _SWIFT_DEFINED_COMPONENTS "${component}" index)
    if(${index} EQUAL -1)
      message(FATAL_ERROR "unknown install component: ${component}")
    endif()

    string(TOUPPER "${component}" var_name_piece)
    string(REPLACE "-" "_" var_name_piece "${var_name_piece}")
    set("${result_var_name}" "${SWIFT_INSTALL_${var_name_piece}}" PARENT_SCOPE)
  endif()
endfunction()

# swift_install_in_component(<COMPONENT NAME>
#   <same parameters as install()>)
#
# Executes the specified installation actions if the named component is
# requested to be installed.
#
# This function accepts the same parameters as install().
function(swift_install_in_component component)
  precondition(component MESSAGE "Component name is required")

  swift_is_installing_component("${component}" is_installing)
  if(is_installing)
    install(${ARGN})
  endif()
endfunction()

macro(swift_configure_install_components install_components)
  foreach(component ${_SWIFT_DEFINED_COMPONENTS})
    string(TOUPPER "${component}" var_name_piece)
    string(REPLACE "-" "_" var_name_piece "${var_name_piece}")
    set(SWIFT_INSTALL_${var_name_piece} FALSE)
  endforeach()

  foreach(component ${install_components})
    list(FIND _SWIFT_DEFINED_COMPONENTS "${component}" index)
    if(${index} EQUAL -1)
      message(FATAL_ERROR "unknown install component: ${component}")
    endif()

    string(TOUPPER "${component}" var_name_piece)
    string(REPLACE "-" "_" var_name_piece "${var_name_piece}")
    set(SWIFT_INSTALL_${var_name_piece} TRUE)
  endforeach()
endmacro()
