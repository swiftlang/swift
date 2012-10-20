add_swift_executable(httpd
  httpd.swift)

target_link_libraries(httpd)

# FIXME: Eventually this should be part of add_swift_executable
# if any of the sources are Swift files.
add_dependencies(httpd
  swift_runtime
  )

install(TARGETS httpd
  RUNTIME DESTINATION bin)
