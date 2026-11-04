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

emit_swift_interface(swift_COM_Concurrency)
install(TARGETS swift_COM_Concurrency
  EXPORT SwiftSupplementalTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  FILE_SET swift_modules
    COMPONENT ${PROJECT_NAME}_development
    DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}")

embed_manifest(swift_COM_Concurrency)
