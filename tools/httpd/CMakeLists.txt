add_swift_executable(httpd
  httpd.swift)

target_link_libraries(httpd)

install(TARGETS httpd
  RUNTIME DESTINATION bin)
