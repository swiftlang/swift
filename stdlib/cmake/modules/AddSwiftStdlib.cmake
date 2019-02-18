
include(AddSwift)

# Add an executable for each target variant. Executables are given suffixes
# with the variant SDK and ARCH.
#
# See add_swift_executable for detailed documentation.
#
# Additional parameters:
#   [LINK_FAT_LIBRARIES lipo_target1 ...]
#     Fat libraries to link with.
function(add_swift_target_executable name)
  # Parse the arguments we were given.
  cmake_parse_arguments(SWIFTEXE_TARGET
    "EXCLUDE_FROM_ALL;;BUILD_WITH_STDLIB"
    ""
    "DEPENDS;LLVM_COMPONENT_DEPENDS;LINK_FAT_LIBRARIES"
    ${ARGN})

  set(SWIFTEXE_TARGET_SOURCES ${SWIFTEXE_TARGET_UNPARSED_ARGUMENTS})

  translate_flag(${SWIFTEXE_TARGET_EXCLUDE_FROM_ALL}
      "EXCLUDE_FROM_ALL"
      SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG)

  # All Swift executables depend on the standard library.
  list(APPEND SWIFTEXE_TARGET_LINK_FAT_LIBRARIES swiftCore)
  # All Swift executables depend on the swiftSwiftOnoneSupport library.
  list(APPEND SWIFTEXE_TARGET_DEPENDS swiftSwiftOnoneSupport)

  if(NOT "${SWIFT_BUILD_STDLIB}")
    list(REMOVE_ITEM SWIFTEXE_TARGET_LINK_FAT_LIBRARIES
        swiftCore)
  endif()

  foreach(sdk ${SWIFT_SDKS})
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
      set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")

      set(SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG_CURRENT
          ${SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG})
      if(NOT "${VARIANT_SUFFIX}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SUFFIX}")
        # By default, don't build executables for target SDKs to avoid building
        # target stdlibs.
        set(SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG_CURRENT "EXCLUDE_FROM_ALL")
      endif()

      if(SWIFTEXE_TARGET_BUILD_WITH_STDLIB)
        add_dependencies("swift-test-stdlib${VARIANT_SUFFIX}" ${VARIANT_NAME})
      endif()

      # Don't add the ${arch} to the suffix.  We want to link against fat
      # libraries.
      _list_add_string_suffix(
          "${SWIFTEXE_TARGET_DEPENDS}"
          "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}"
          SWIFTEXE_TARGET_DEPENDS_with_suffix)
      _add_swift_executable_single(
          ${VARIANT_NAME}
          ${SWIFTEXE_TARGET_SOURCES}
          DEPENDS ${SWIFTEXE_TARGET_DEPENDS_with_suffix}
          LLVM_COMPONENT_DEPENDS ${SWIFTEXE_TARGET_LLVM_COMPONENT_DEPENDS}
          SDK "${sdk}"
          ARCHITECTURE "${arch}"
          LINK_FAT_LIBRARIES ${SWIFTEXE_TARGET_LINK_FAT_LIBRARIES}
          ${SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG_CURRENT})

      if(${sdk} IN_LIST SWIFT_APPLE_PLATFORMS)
        add_custom_command_target(unused_var2
         COMMAND "codesign" "-f" "-s" "-" "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}"
         CUSTOM_TARGET_NAME "${VARIANT_NAME}_signed"
         OUTPUT "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}_signed"
         DEPENDS ${VARIANT_NAME})
      else()
        # No code signing on other platforms.
        add_custom_command_target(unused_var2
         CUSTOM_TARGET_NAME "${VARIANT_NAME}_signed"
         OUTPUT "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}_signed"
         DEPENDS ${VARIANT_NAME})
       endif()
    endforeach()
  endforeach()
endfunction()
