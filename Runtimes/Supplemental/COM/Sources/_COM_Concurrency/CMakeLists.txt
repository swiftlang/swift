add_library(swift_COM_Concurrency
  COMActivation.swift
  "COMContext+Concurrency.swift")
set_target_properties(swift_COM_Concurrency PROPERTIES
  Swift_MODULE_NAME _COM_Concurrency)
target_link_libraries(swift_COM_Concurrency PRIVATE
  swiftCore
  swift_Concurrency
  swiftSynchronization
  swiftCOM
  $<$<PLATFORM_ID:Windows>:swiftWinSDK>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swift_COM_Concurrency
  EXPORT SwiftSupplementalTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_COM_Concurrency)
if(${PROJECT_NAME}_ENABLE_SYMBOL_GRAPHS)
  target_link_libraries(swift_COM_Concurrency PRIVATE SwiftCore_SymbolGraph)
  set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/symbol-graphs")
endif()
install_swift_interface(swift_COM_Concurrency)

embed_manifest(swift_COM_Concurrency)
