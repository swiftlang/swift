# This file contains cmake configuration specifically related to support for the
# Xcode generator in CMake.

function(escape_path_for_xcode config path result_var_name)
  # If we are not using the Xcode generator, be defensive and early exit.
  if (NOT XCODE)
    set(${result_var_name} "${path}" PARENT_SCOPE)
    return()
  endif()

  # Hack to deal with the fact that paths contain the build-time
  # variables. Note that this fix is Xcode-specific.
  string(REPLACE "$(CONFIGURATION)" "${config}" result "${path}")
  string(REPLACE "$(EFFECTIVE_PLATFORM_NAME)" "" result "${result}")
  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()

function(check_imported_target_has_imported_configuration target config)
  get_target_property(IMPORTED_CONFIGS_LIST ${target} IMPORTED_CONFIGURATIONS)
  if ("${IMPORTED_CONFIGS_LIST}" STREQUAL "IMPORTED_CONFIGS_LIST-NOTFOUND")
    message(FATAL_ERROR "No import configuration of ${target} specified?!")
  endif()

  if(NOT "${config}" IN_LIST IMPORTED_CONFIGS_LIST)
    message(FATAL_ERROR "${target} does not have imported config '${config}'?! Instead: ${IMPORTED_CONFIGS_LIST}")
  endif()
endfunction()

function(fixup_imported_target_property_for_xcode target property llvm_build_type)
  set(FULL_PROP_NAME "${property}_${llvm_build_type}")

  # First try to lookup the value associated with the "real build type".
  get_target_property(PROP_VALUE ${target} ${FULL_PROP_NAME})

  # If the property is unspecified, return.
  if ("${PROP_VALUE}" STREQUAL "PROP_VALUE-NOTFOUND")
    return()
  endif()

  # Otherwise for each cmake configuration that is not real_build_type, hardcode
  # its value to be PROP_VALUE.
  foreach(build_type ${CMAKE_CONFIGURATION_TYPES})
    string(TOUPPER "${build_type}" build_type_upper)
    if ("${build_type_upper}" STREQUAL "${llvm_build_type}")
      continue()
    endif()
    set(SWIFT_BUILD_PROPERTY_NAME "${property}_${build_type_upper}")
    set_target_properties(${target} PROPERTIES "${SWIFT_BUILD_PROPERTY_NAME}" "${PROP_VALUE}")
  endforeach()
endfunction()

# When building with Xcode, we only support compiling against the LLVM
# configuration that was specified by build-script. This becomes a problem since
# if we compile LLVM-Release and Swift-Debug, Swift is going to look in the
# Debug, not the Release folder for LLVM's code and thus will be compiling
# against an unintended set of libraries, if those libraries exist at all.
#
# Luckily, via LLVMConfig.cmake, we know the configuration that LLVM was
# compiled in, so we can grab the imported location for that configuration and
# splat it across the other configurations as well.
function(fix_imported_targets_for_xcode imported_targets)
  string(TOUPPER "${LLVM_BUILD_TYPE}" LLVM_BUILD_TYPE_UPPER)

  # This is the set of configuration specific cmake properties that are
  # supported for imported targets in cmake 3.4.3. Sadly, beyond hacks, it seems
  # that there is no way to dynamically query the list of set properties of a
  # target.
  #
  # *NOTE* In fixup_imported_target_property_for_xcode, we add the _${CONFIG}
  # *suffix.
  set(imported_target_properties
    IMPORTED_IMPLIB
    IMPORTED_LINK_DEPENDENT_LIBRARIES
    IMPORTED_LINK_INTERFACE_LANGUAGES
    IMPORTED_LINK_INTERFACE_LIBRARIES
    IMPORTED_LINK_INTERFACE_MULTIPLICITY
    IMPORTED_LOCATION
    IMPORTED_NO_SONAME
    IMPORTED_SONAME)

  foreach(target ${imported_targets})
    if (NOT TARGET ${target})
      message(FATAL_ERROR "${target} is not a target?!")
    endif()

    # First check that we actually imported the configuration that LLVM said
    # that we did. This is just a sanity check.
    check_imported_target_has_imported_configuration(${target} ${LLVM_BUILD_TYPE_UPPER})

    # Then loop through all of the imported properties and translate.
    foreach(property ${imported_target_properties})
      fixup_imported_target_property_for_xcode(
        ${target} ${property} ${LLVM_BUILD_TYPE_UPPER})
    endforeach()
  endforeach()
endfunction()

# Common additional cmake project config for Xcode.
macro(swift_common_xcode_cxx_config)
  # Force usage of Clang.
  set(CMAKE_XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0"
      CACHE STRING "Xcode Compiler")
  # Use C++'14.
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LANGUAGE_STANDARD "c++14"
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
