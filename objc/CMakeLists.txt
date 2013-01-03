# Installation for Swift modules.
set(SWIFTLIB_DIR "${CMAKE_BINARY_DIR}/lib/${CMAKE_CFG_INTDIR}")
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

install_swift_module(ObjC.swift)

add_subdirectory(Foundation)
add_subdirectory(ScriptingBridge)
