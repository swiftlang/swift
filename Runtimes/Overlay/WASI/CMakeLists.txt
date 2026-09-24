
add_subdirectory(clang)
add_subdirectory(WASILibc)

if(NOT BUILD_SHARED_LIBS)
  install(FILES Resources/static-executable-args.lnk
    COMPONENT SwiftOverlay_runtime
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift_static/${SwiftOverlay_PLATFORM_SUBDIR}")
endif()
