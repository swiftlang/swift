
add_subdirectory(COM)
add_subdirectory(_COM_Concurrency)

install(FILES "Concurrency.swiftoverlay"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>/${${PROJECT_NAME}_PLATFORM_SUBDIR}/COM.swiftcrossimport")
