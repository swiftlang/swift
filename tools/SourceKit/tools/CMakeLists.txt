set(SOURCEKITD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/sourcekitd)

include_directories(
  ${SOURCEKITD_SOURCE_DIR}/include
)

add_swift_lib_subdirectory(sourcekitd)
add_swift_tool_subdirectory(sourcekitd-test)
if(LibEdit_FOUND AND LibEdit_HAS_UNICODE)
  add_swift_tool_subdirectory(sourcekitd-repl)
endif()
add_swift_tool_subdirectory(complete-test)
