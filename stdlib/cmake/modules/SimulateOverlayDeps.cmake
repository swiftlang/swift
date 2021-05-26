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

function(setup_use_of_overlays_from_sdks all_overlays overlays_to_fake)
  message(STATUS "Cleaning up stale overlays in build folder: ${all_overlays}")
  foreach(overlay IN LISTS all_overlays)
    remove_overlay_from_build(${overlay} TARGET_SDKS ${SWIFT_SDKS})
  endforeach()
    
  message(STATUS "Overlays to add fake dependencies for: ${overlays_to_fake}")
  foreach(overlay IN LISTS overlays_to_fake)
    add_overlay_targets(${overlay} TARGET_SDKS ${SWIFT_SDKS})
  endforeach()
endfunction()

function(add_overlay_targets overlay)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs "TARGET_SDKS")

  cmake_parse_arguments(AOT "${options}" "${oneValueArgs}"
                                "${multiValueArgs}" ${ARGN} )

  foreach(sdk ${AOT_TARGET_SDKS})
    set(sdk_name ${SWIFT_SDK_${sdk}_LIB_SUBDIR})
    set(overlay_dylib_target swift${overlay}-${sdk_name})
    add_library(${overlay_dylib_target} SHARED IMPORTED GLOBAL)
    set_property(TARGET ${overlay_dylib_target}
      PROPERTY IMPORTED_LOCATION ${SWIFT_SDK_${sdk}_PATH}/usr/lib/swift/libswift${overlay}.tbd)

    add_overlay_targets_single(
      ${overlay}
      TARGET_SDK ${sdk}
      DYLIB_ALIAS ${overlay_dylib_target})
  endforeach()
endfunction()

function(add_overlay_targets_single overlay)
  set(options)
  set(oneValueArgs "TARGET_SDK" "DYLIB_ALIAS")
  set(multiValueArgs)

  cmake_parse_arguments(AOT "${options}" "${oneValueArgs}"
                                "${multiValueArgs}" ${ARGN} )

  set(sdk ${AOT_TARGET_SDK})
  set(sdk_name ${SWIFT_SDK_${sdk}_LIB_SUBDIR})
  set(sdk_supported_archs
    ${SWIFT_SDK_${sdk}_ARCHITECTURES}
    ${SWIFT_SDK_${sdk}_MODULE_ARCHITECTURES})
  list(REMOVE_DUPLICATES sdk_supported_archs)

  foreach(arch ${sdk_supported_archs})
    set(variant_suffix "${sdk_name}-${arch}")

    set(overlay_dylib_target swift${overlay}-${variant_suffix})
    add_library(${overlay_dylib_target} ALIAS ${AOT_DYLIB_ALIAS})

    set(overlay_swiftmodule_target swift${overlay}-swiftmodule-${variant_suffix})
    add_custom_target(${overlay_swiftmodule_target})
    if(SWIFT_ENABLE_MACCATALYST AND sdk STREQUAL "OSX")
      set(overlay_maccatalyst_swiftmodule_target swift${overlay}-swiftmodule-maccatalyst-${arch})
      add_custom_target(${overlay_maccatalyst_swiftmodule_target})
    endif()
  endforeach()
endfunction()

function(remove_overlay_from_build overlay)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs "TARGET_SDKS")

  cmake_parse_arguments(ROFB "${options}" "${oneValueArgs}"
                                "${multiValueArgs}" ${ARGN} )

  foreach(sdk ${ROFB_TARGET_SDKS})
    remove_overlay_from_build_single(
      ${overlay}
      TARGET_SDK ${sdk})
  endforeach()
endfunction()

function(remove_overlay_from_build_single overlay)
  set(options)
  set(oneValueArgs "TARGET_SDK")
  set(multiValueArgs)

  cmake_parse_arguments(ROFB "${options}" "${oneValueArgs}"
                                "${multiValueArgs}" ${ARGN} )

  set(sdk ${ROFB_TARGET_SDK})
  set(sdk_name ${SWIFT_SDK_${sdk}_LIB_SUBDIR})

  file(GLOB dylibs LIST_DIRECTORIES false ${SWIFTLIB_DIR}/${sdk_name}/*/libswift${overlay}.dylib
    ${SWIFTLIB_DIR}/${sdk_name}/libswift${overlay}.dylib)
  file(REMOVE_RECURSE ${dylibs})
  file(REMOVE_RECURSE ${SWIFTLIB_DIR}/${sdk_name}/${overlay}.swiftmodule)
endfunction()
