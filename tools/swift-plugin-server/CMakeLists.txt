if (SWIFT_BUILD_SWIFT_SYNTAX)
  # override the remote SwiftSystem dependency in WasmKit
  # TODO: create new variables instead of piggybacking off of SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE
  file(TO_CMAKE_PATH "${SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE}" swift_syntax_path)
  FetchContent_Declare(SwiftSystem SOURCE_DIR "${swift_syntax_path}/../swift-system")

  set(WASMKIT_BUILD_CLI OFF)
  FetchContent_Declare(WasmKit SOURCE_DIR "${swift_syntax_path}/../wasmkit")
  FetchContent_MakeAvailable(WasmKit)

  add_pure_swift_host_tool(swift-plugin-server
    Sources/swift-plugin-server/swift-plugin-server.swift
    Sources/swift-plugin-server/WasmEngine.swift
    Sources/swift-plugin-server/WasmKitEngine.swift
    Sources/swift-plugin-server/WasmMessageHandler.swift
    SWIFT_COMPONENT
      compiler
    SWIFT_DEPENDENCIES
      SwiftCompilerPluginMessageHandling
      SwiftLibraryPluginProvider
      WasmKitWASI
  )
endif()
