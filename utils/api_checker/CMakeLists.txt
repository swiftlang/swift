install(FILES "swift-api-checker.py"
        DESTINATION "bin"
        COMPONENT tools)
install(FILES "swift-api-checker.py"
        DESTINATION "bin"
        COMPONENT toolchain-tools)

install(DIRECTORY "sdk-module-lists"
        DESTINATION "bin"
        COMPONENT tools)
install(DIRECTORY "sdk-module-lists"
        DESTINATION "bin"
        COMPONENT toolchain-tools)
