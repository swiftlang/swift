
include(AddSwift)

# Add an executable for each target variant. Executables are given suffixes
# with the variant SDK and ARCH.
#
# See add_swift_executable for detailed documentation.
function(add_swift_target_executable name)
  # Parse the arguments we were given.
  cmake_parse_arguments(SWIFTEXE_TARGET
    "EXCLUDE_FROM_ALL;;BUILD_WITH_STDLIB"
    ""
    "DEPENDS;LLVM_LINK_COMPONENTS;LINK_LIBRARIES"
    ${ARGN})

  set(SWIFTEXE_TARGET_SOURCES ${SWIFTEXE_TARGET_UNPARSED_ARGUMENTS})

  if(SWIFTEXE_TARGET_EXCLUDE_FROM_ALL)
    message(SEND_ERROR "${name} is using EXCLUDE_FROM_ALL which is deprecated.")
  endif()

  # All Swift executables depend on the standard library.
  list(APPEND SWIFTEXE_TARGET_LINK_LIBRARIES swiftCore)
  # All Swift executables depend on the swiftSwiftOnoneSupport library.
  list(APPEND SWIFTEXE_TARGET_DEPENDS swiftSwiftOnoneSupport)

  foreach(sdk ${SWIFT_SDKS})
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
      set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")

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
          LLVM_LINK_COMPONENTS ${SWIFTEXE_TARGET_LLVM_LINK_COMPONENTS}
          SDK "${sdk}"
          ARCHITECTURE "${arch}")

      _list_add_string_suffix(
          "${SWIFTEXE_TARGET_LINK_LIBRARIES}"
          "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}"
          SWIFTEXE_TARGET_LINK_LIBRARIES_TARGETS)
      target_link_libraries(${VARIANT_NAME} PRIVATE
          ${SWIFTEXE_TARGET_LINK_LIBRARIES_TARGETS})

      if(NOT "${VARIANT_SUFFIX}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SUFFIX}")
        # By default, don't build executables for target SDKs to avoid building
        # target stdlibs.
        set_target_properties(${VARIANT_NAME} PROPERTIES
          EXCLUDE_FROM_ALL TRUE)
      endif()

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
