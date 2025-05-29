# macCatalystUtils.cmake
#
# Utility functions for macCatalyst support in Swift.


# Include guard
if(MACCATALYST_UTILS_INCLUDED)
  return()
endif()

set(MACCATALYST_UTILS_INCLUDED TRUE)


# -----------------------------------------------------------------------------

# List of all valid macCatalyst build flavors
set(MACCATALYST_BUILD_FLAVORS "ios-like" "macos-like" "zippered" "unzippered-twin")


# Sets out_var with the macCatalyst build flavor if macCatalyst is enabled and building
# for the OSX sdk.
function(get_maccatalyst_build_flavor out_var sdk flavor)
  if(SWIFT_ENABLE_MACCATALYST AND sdk STREQUAL "OSX")
    if(flavor IN_LIST MACCATALYST_BUILD_FLAVORS)
      set("${out_var}" "${flavor}" PARENT_SCOPE)
    elseif(NOT flavor STREQUAL "")
      message(FATAL_ERROR "Invalid MACCATALYST_BUILD_FLAVOR: ${flavor}")
    else()
      # Unset the variable to indicate the absence of a build flavor
      unset("${out_var}" PARENT_SCOPE)
    endif()
  else()
    # Unset the variable to indicate macCatalyst is not enabled
    unset("${out_var}" PARENT_SCOPE)
  endif()
endfunction()

# Form a versioned target triple for the given SDK.
function(get_versioned_target_triple target_out_var sdk arch version)
  if (SWIFT_SDK_${sdk}_IS_SIMULATOR)
    # The version goes before the "-simulator".
    set(target "${SWIFT_SDK_${sdk}_ARCH_${arch}_TRIPLE}")
    string(REPLACE "-simulator" "" target "${target}")
    set(target "${target}${version}-simulator")
  else ()
    set(target "${SWIFT_SDK_${sdk}_ARCH_${arch}_TRIPLE}${version}")
  endif()

  set(${target_out_var} "${target}" PARENT_SCOPE)
endfunction()

# Sets target_out_var to the target triple for the given SDK and maccatalyst flavor.
# For zippered flavors also sets the target_variant_out_var. For other
# flavors the target_variant_out_var is unset, causing it to be undefined.
function(get_target_triple target_out_var target_variant_out_var sdk arch)
  # parse args
  set(option_args)
  set(single_value_args MACCATALYST_BUILD_FLAVOR DEPLOYMENT_VERSION)
  set(multi_value_args)
  cmake_parse_arguments(TARGET
    "${option_args}"
    "${single_value_args}"
    "${multi_value_args}"
    ${ARGN})

  set(deployment_version "${TARGET_DEPLOYMENT_VERSION}")

  # Default target triple
  get_versioned_target_triple(target ${sdk} ${arch} "${deployment_version}")

  set(target_variant)

  get_maccatalyst_build_flavor(maccatalyst_build_flavor
    "${sdk}" "${TARGET_MACCATALYST_BUILD_FLAVOR}")

  if(maccatalyst_build_flavor STREQUAL "ios-like")
    set(target "${arch}-apple-ios${SWIFT_DARWIN_DEPLOYMENT_VERSION_MACCATALYST}-macabi")
  elseif(maccatalyst_build_flavor STREQUAL "macos-like")
    # Use the default macOS triple.
  elseif(maccatalyst_build_flavor STREQUAL "zippered")
    set(target "${arch}-apple-macosx${deployment_version}")
    set(target_variant "${arch}-apple-ios${SWIFT_DARWIN_DEPLOYMENT_VERSION_MACCATALYST}-macabi")
  elseif(maccatalyst_build_flavor STREQUAL "unzippered-twin")
    # Use the default triple for now
  endif()

  set(${target_out_var} "${target}" PARENT_SCOPE)
  set(${target_variant_out_var} "${target_variant}" PARENT_SCOPE)
endfunction()


# Removes all instances of `-${flag} <arg>` from an input list of flags
function(remove_given_flag flags_var flag_name)
  set(output_flags)

  set(seen_flag FALSE)
  foreach(flag ${${flags_var}})
    # Skip flag argument
    if(seen_flag)
      set(seen_flag FALSE)
      continue()
    endif()

    # Skip flag
    if(flag STREQUAL "-${flag_name}")
      set(seen_flag TRUE)
      continue()
    endif()

    list(APPEND output_flags "${flag}")
  endforeach()

  set("${flags_var}" "${output_flags}" PARENT_SCOPE)
endfunction()
