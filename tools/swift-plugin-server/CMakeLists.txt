if (SWIFT_BUILD_SWIFT_SYNTAX)
  # _swiftCSwiftPluginServer is just a C support library for swift-plugin-server
  # Don't bother to create '.a' for that.
  add_swift_host_library(_swiftCSwiftPluginServer OBJECT
    Sources/CSwiftPluginServer/PluginServer.cpp
  )
  target_link_libraries(_swiftCSwiftPluginServer PRIVATE
    swiftDemangling
  )
  target_include_directories(_swiftCSwiftPluginServer PUBLIC
    Sources/CSwiftPluginServer/include
  )

  add_pure_swift_host_tool(swift-plugin-server
    Sources/swift-plugin-server/swift-plugin-server.swift
    DEPENDENCIES
      swiftDemangling
      $<TARGET_OBJECTS:_swiftCSwiftPluginServer>
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftSyntaxMacros
      SwiftSyntaxMacroExpansion
      SwiftCompilerPluginMessageHandling
      swiftLLVMJSON
  )
  target_include_directories(swift-plugin-server PRIVATE
    Sources/CSwiftPluginServer/include
  )

  add_pure_swift_host_tool(swift-wasm-plugin-server
    Sources/swift-wasm-plugin-server/swift-wasm-plugin-server.swift
    DEPENDENCIES
      $<TARGET_OBJECTS:_swiftCSwiftPluginServer>
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftCompilerPluginMessageHandling
      swiftLLVMJSON
  )
  target_include_directories(swift-wasm-plugin-server PRIVATE
    Sources/CSwiftPluginServer/include
  )
  add_custom_command(TARGET swift-wasm-plugin-server POST_BUILD COMMAND
    codesign -fs - $<TARGET_FILE:swift-wasm-plugin-server> --entitlements ${CMAKE_CURRENT_SOURCE_DIR}/allow-jit.plist
  )
endif()
