#===--- OverlayXcodeExternalProject.cmake - build overlays with Xcode   --===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#
include(ExternalProject)


# Setup CMake targets and dependencies
# for overlays built with add_overlay_xcode_project
# that existing code expects,
# so to minimize disruption during migration to
# building with Xcode and allow to switch
# between implementations
function(add_overlay_targets overlay)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs "TARGET_SDKS")

  cmake_parse_arguments(AOT "${options}" "${oneValueArgs}"
                                "${multiValueArgs}" ${ARGN} )

  foreach(sdk ${AOT_TARGET_SDKS})
    add_overlay_targets_single(
      ${overlay}
      TARGET_SDK ${sdk})
  endforeach()
endfunction()

function(add_overlay_targets_single overlay)
  set(options)
  set(oneValueArgs "TARGET_SDK")
  set(multiValueArgs)

  cmake_parse_arguments(AOT "${options}" "${oneValueArgs}"
                                "${multiValueArgs}" ${ARGN} )

  set(sdk ${AOT_TARGET_SDK})
  set(sdk_name ${SWIFT_SDK_${sdk}_LIB_SUBDIR})
  set(sdk_supported_archs
    ${SWIFT_SDK_${sdk}_ARCHITECTURES}
    ${SWIFT_SDK_${sdk}_MODULE_ARCHITECTURES})
  list(REMOVE_DUPLICATES sdk_supported_archs)
  #set(xcode_overlay_target_name ${overlay}Overlay-${sdk_name})

  foreach(arch ${sdk_supported_archs})
    set(variant_suffix "${sdk_name}-${arch}")

    set(overlay_dylib_target swift${overlay}-${variant_suffix})
    add_library(${overlay_dylib_target} SHARED IMPORTED GLOBAL)
    set_property(TARGET ${overlay_dylib_target}
      PROPERTY IMPORTED_LOCATION ${SWIFT_SDK_${sdk}_ARCH_${arch}_PATH}/usr/lib/swift/libswift${overlay}.tbd)
    #add_dependencies(${overlay_dylib_target} ${xcode_overlay_target_name})

    set(overlay_swiftmodule_target swift${overlay}-swiftmodule-${variant_suffix})
    add_custom_target(${overlay_swiftmodule_target})
    #add_dependencies(${overlay_swiftmodule_target} ${xcode_overlay_target_name})
    if(SWIFT_ENABLE_MACCATALYST AND sdk STREQUAL "OSX")
      set(overlay_maccatalyst_swiftmodule_target swift${overlay}-swiftmodule-maccatalyst-${arch})
      add_custom_target(${overlay_maccatalyst_swiftmodule_target})
      #add_dependencies(${overlay_maccatalyst_swiftmodule_target} ${xcode_overlay_target_name})
    endif()

    #add_dependencies(swift-stdlib-${variant_suffix} ${xcode_overlay_target_name})
  endforeach()

  #add_dependencies(sdk-overlay ${xcode_overlay_target_name})
endfunction()
