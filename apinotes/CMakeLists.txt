# List the modules that have API notes associated with SDK overlays.
set(SWIFT_API_NOTES_OVERLAY_MODULES
  AppKit
  Foundation
  ObjectiveC
  UIKit
  )

# List the modules that have API notes not asociated with SDK overlays.
set(SWIFT_API_NOTES_NON_OVERLAY_MODULES
  NotificationCenter
  )

# Put SWIFT_API_NOTES_OVERLAY_MODULES and
# SWIFT_API_NOTES_NON_OVERLAY_MODULES into the parent scope, because
# they are consumed by the Swift build.
set(SWIFT_API_NOTES_OVERLAY_MODULES ${SWIFT_API_NOTES_OVERLAY_MODULES}
  PARENT_SCOPE)
set(SWIFT_API_NOTES_NON_OVERLAY_MODULES ${SWIFT_API_NOTES_NON_OVERLAY_MODULES}
  PARENT_SCOPE)

# Consistency check: make sure all ".apinotes" files are reflected in
# one of the above lists.
file(GLOB ALL_API_NOTES_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.apinotes)
foreach(file ${ALL_API_NOTES_SOURCES})
  get_filename_component(module ${file} NAME_WE)
  list(FIND SWIFT_API_NOTES_OVERLAY_MODULES ${module} overlay_index)
  list(FIND SWIFT_API_NOTES_NON_OVERLAY_MODULES ${module} non_overlay_index)

  if (${overlay_index} GREATER -1)
    if (${non_overlay_index} GREATER -1)
      message(SEND_ERROR "module '${module}' appears in both SWIFT_API_NOTES_OVERLAY_MODULES and SWIFT_API_NOTES_NON_OVERLAY_MODULES")
    endif()
  endif()

  if (${overlay_index} EQUAL -1)
    if (${non_overlay_index} EQUAL -1)
      message(SEND_ERROR "error: module '${module}' does not appear in either SWIFT_API_NOTES_OVERLAY_MODULES or SWIFT_API_NOTES_NON_OVERLAY_MODULES")
    endif()
  endif()    
endforeach()
