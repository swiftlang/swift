configure_file("${SwiftCore_SWIFTC_SOURCE_DIR}/utils/availability-macros.def"
               "${CMAKE_CURRENT_BINARY_DIR}/availability-macros.def"
               COPYONLY)
file(STRINGS "${CMAKE_CURRENT_BINARY_DIR}/availability-macros.def" availability_defs)
list(FILTER availability_defs EXCLUDE REGEX "^\\s*(#.*)?$")
foreach(def ${availability_defs})
  add_compile_options("$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -define-availability -Xfrontend \"${def}\">")
endforeach()
