set(SHARED_LIBRARY ON)

add_swift_library(swiftScriptingBridge
  ScriptingBridge.swift
  DEPENDS swift_stdlib)

install_swift_module(ScriptingBridge.swift)

# Link against frameworks
target_link_libraries(swiftScriptingBridge
  "-framework ScriptingBridge"
  "-fobjc-link-runtime")
