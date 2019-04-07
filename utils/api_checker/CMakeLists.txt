swift_install_in_either_component(tools toolchain-tools
  FILES "swift-api-checker.py"
  DESTINATION "bin")
swift_install_in_either_component(tools toolchain-tools
  DIRECTORY "sdk-module-lists"
  DESTINATION "bin")
