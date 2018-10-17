cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

swift_add_target_library(swiftNetwork ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Network.swift
  NWConnection.swift
  NWEndpoint.swift
  NWError.swift
  NWListener.swift
  NWParameters.swift
  NWPath.swift
  NWProtocolIP.swift
  NWProtocol.swift
  NWProtocolTCP.swift
  NWProtocolTLS.swift
  NWProtocolUDP.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR # WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin Dispatch Foundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin Dispatch Foundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin Dispatch Foundation ObjectiveC # auto-updated
  # SWIFT_MODULE_DEPENDS_WATCHOS Darwin Dispatch Foundation ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS_WEAK Network

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_NETWORK_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_NETWORK_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_NETWORK_TVOS}
  # DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_NETWORK_WATCHOS}
)
