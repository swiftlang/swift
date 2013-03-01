# Installation for Swift modules.
function(install_swift_module items)
  foreach(item ${items})
    add_custom_command(
      OUTPUT "${SWIFTLIB_DIR}/swift/${item}"
      COMMAND mkdir -p "${SWIFTLIB_DIR}/swift"
      COMMAND cp "${item}" "${SWIFTLIB_DIR}/swift/${item}"
      DEPENDS "${item}"
      COMMENT "Installing ${item}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
  endforeach()
endfunction()

# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)
add_swift_library(swiftObjectiveC
  ObjectiveC.swift
  DEPENDS swift_stdlib)

# Link against the system dylib.
target_link_libraries(swiftObjectiveC
  "-lobjc")

install_swift_module(ObjectiveC.swift)

add_subdirectory(Foundation)
add_subdirectory(AppKit)
