set(SOURCEKITD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/sourcekitd)

include_directories(
  ${SOURCEKITD_SOURCE_DIR}/include
)

add_subdirectory(sourcekitd)
add_subdirectory(sourcekitd-test)
add_subdirectory(sourcekitd-repl)
add_subdirectory(complete-test)
