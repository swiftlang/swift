if (SWIFT_BUILD_SWIFT_SYNTAX)
  # hack: override macOS deployment target to 10.15.4
  # since that's the minimum required by WasmKit
  set(SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX "10.15.4")
  include(DarwinSDKs)
  swift_get_host_triple(SWIFT_HOST_TRIPLE)

  file(TO_CMAKE_PATH "${SWIFT_PATH_TO_WASMKIT_SOURCE}" wasmkit_path)
  FetchContent_Declare(WasmKit SOURCE_DIR "${wasmkit_path}")
  FetchContent_MakeAvailable(WasmKit)

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

  add_pure_swift_host_library(SwiftPluginServerSupport EMIT_MODULE
    Sources/SwiftPluginServerSupport/SwiftPluginServerSupport.swift
    DEPENDENCIES
      $<TARGET_OBJECTS:_swiftCSwiftPluginServer>
  )
  target_include_directories(SwiftPluginServerSupport PRIVATE
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
      SwiftPluginServerSupport
  )
  target_include_directories(swift-plugin-server PRIVATE
    Sources/CSwiftPluginServer/include
  )

  add_pure_swift_host_tool(swift-wasm-plugin-server
    Sources/swift-wasm-plugin-server/swift-wasm-plugin-server.swift
    Sources/swift-wasm-plugin-server/JSCWasmEngine.swift
    Sources/swift-wasm-plugin-server/WasmEngine.swift
    Sources/swift-wasm-plugin-server/WasmKitEngine.swift
    DEPENDENCIES
      $<TARGET_OBJECTS:_swiftCSwiftPluginServer>
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftCompilerPluginMessageHandling
      swiftLLVMJSON
      SwiftPluginServerSupport
      WASI WasmKit WasmKitWASI WasmTypes
  )
  target_include_directories(swift-wasm-plugin-server PRIVATE
    Sources/CSwiftPluginServer/include
  )
  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    add_custom_command(TARGET swift-wasm-plugin-server POST_BUILD COMMAND
      codesign -fs - $<TARGET_FILE:swift-wasm-plugin-server> --entitlements ${CMAKE_CURRENT_SOURCE_DIR}/allow-jit.plist
    )
  endif()
endif()
