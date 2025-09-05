# SwiftTestUtils.cmake
#
# Utility functions for Swift testing targets

# Get the possible build flavors for testing
function(get_swift_test_build_flavors build_flavors_out_var sdk)
  set(build_flavors "default")
  if(SWIFT_ENABLE_MACCATALYST AND "${sdk}" STREQUAL "OSX")
    list(APPEND build_flavors "ios-like")
  endif()

  set(${build_flavors_out_var} ${build_flavors} PARENT_SCOPE)
endfunction()

# Get the variant suffix for test targets and folders
function(get_swift_test_variant_suffix variant_suffix_out_var sdk arch build_flavor)
  if(build_flavor STREQUAL "ios-like")
    set(variant_suffix "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-maccatalyst-${arch}")
  else()
    set(variant_suffix "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
  endif()

  set(${variant_suffix_out_var} "${variant_suffix}" PARENT_SCOPE)
endfunction()


# Get the variant triple for test targets
function(get_swift_test_versioned_target_triple variant_triple_out_var sdk arch build_flavor)
  if(build_flavor STREQUAL "ios-like")
    # Use the macCatalyst target triple and compiler resources for the iOS-like build flavor.
    set(variant_triple "${arch}-apple-ios13.1-macabi")
  else()
    get_versioned_target_triple(variant_triple ${sdk} ${arch} "${SWIFT_SDK_${sdk}_TEST_DEPLOYMENT_VERSION}")
  endif()

  set(${variant_triple_out_var} "${variant_triple}" PARENT_SCOPE)
endfunction()
