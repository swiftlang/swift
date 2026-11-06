
add_subdirectory(clang)
add_subdirectory(Android)
add_subdirectory(Math)

if(NOT BUILD_SHARED_LIBS)
  set(libpthread)
  set(android_libraries -llog)
  set(concurrency_libs)
  swift_runtime_configure_link_file(
    "${PROJECT_SOURCE_DIR}/static-stdlib-args.lnk.in"
    "${CMAKE_CURRENT_BINARY_DIR}/static-stdlib-args.lnk"
    "${SwiftCore_PAGE_SIZE}")
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/static-stdlib-args.lnk"
    COMPONENT SwiftOverlay_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift_static/${SwiftOverlay_PLATFORM_SUBDIR}")
endif()
