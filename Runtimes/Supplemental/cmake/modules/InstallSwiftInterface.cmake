
# Install the generated swift interface files for the target.
function(install_swift_interface target)
  # Swiftmodules are already in the directory structure
  get_target_property(module_name ${target} Swift_MODULE_NAME)
  if(NOT module_name)
    set(module_name ${target})
  endif()

  install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.swiftmodule"
    DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}"
    COMPONENT ${PROJECT_NAME}_development)
endfunction()
