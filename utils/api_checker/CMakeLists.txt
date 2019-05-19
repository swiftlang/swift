swift_install_in_component(FILES "swift-api-checker.py"
                           DESTINATION "bin"
                           COMPONENT toolchain-tools)
swift_install_in_component(DIRECTORY "sdk-module-lists"
                           DESTINATION "bin"
                           COMPONENT toolchain-tools)
