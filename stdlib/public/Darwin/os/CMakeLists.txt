cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")


foreach(sdk ${SWIFT_SDKS})

select_swift_overlay_dependenies(module_depends
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS_OSX Darwin Dispatch XPC ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin ObjectiveC # auto-updated
)

if(${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR
  OR ${sdk} STREQUAL TVOS OR ${sdk} STREQUAL TVOS_SIMULATOR
  OR ${sdk} STREQUAL WATCHOS OR ${sdk} STREQUAL WATCHOS_SIMULATOR)
  list(APPEND module_depends Dispatch) # required in some configurations but not found by tool
endif()

add_swift_target_library_single_sdk(swiftos ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  format.m
  os.m
  os_log.swift
  os_signpost.swift
  os_trace_blob.c
  thunks.mm

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${module_depends}

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_OS_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_OS_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_OS_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_OS_WATCHOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()
