add_swift_executable(cov2json
  ProfileData.cpp
  main.cpp
  LINK_LIBRARIES swiftBasic
  COMPONENT_DEPENDS support object profiledata "option" core)

swift_install_in_component(testsuite-tools
    TARGETS cov2json
    RUNTIME DESTINATION "bin")

