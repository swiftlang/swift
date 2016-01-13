if("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
  add_swift_library(swiftDispatch IS_SDK_OVERLAY
    Dispatch.cpp
    Dispatch.swift

    LINK_FLAGS "-ldispatch")
else()
  add_swift_library(swiftDispatch IS_SDK_OVERLAY
    Dispatch.cpp
    Dispatch.swift

    SWIFT_MODULE_DEPENDS ObjectiveC)
endif()

