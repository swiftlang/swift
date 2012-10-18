add_swift_executable(httpd,httpd.swift)

target_link_libraries(swift)

install(TARGETS httpd
  RUNTIME DESTINATION bin)
