add_swift_library(swiftPhotos ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Photos.swift

  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_IOS Darwin AVFoundation CoreAudio CoreGraphics CoreImage CoreLocation CoreMedia Dispatch Foundation ObjectiveC QuartzCore UIKit # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin AVFoundation CoreAudio CoreGraphics CoreImage CoreLocation CoreMedia Dispatch Foundation ObjectiveC QuartzCore UIKit # auto-updated
  FRAMEWORK_DEPENDS Photos
)
