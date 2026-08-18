add_library(swiftCOM
  "Extensions/String+Extensions.swift"
  CLSID.swift
  COMActivatable.swift
  COMActivation.swift
  COMActivationOptions.swift
  COMAggregate.swift
  COMContext.swift
  COMError.swift
  COMInterface.swift
  COMInterfaceResolution.swift
  COMInterfaceResolver.swift
  COMServerInfo.swift
  GUID.swift
  HRESULT.swift
  IErrorInfo.swift
  IID.swift
  ISwiftObject.swift
  IUnknown.swift
  "IUnknown+Aggregation.swift"
  "IUnknown+Identity.swift"
  ManagedObject.swift
  SwiftRuntime.swift
  TearOffInterfaces.swift
  ThreadingModel.swift)
set_target_properties(swiftCOM PROPERTIES
  Swift_MODULE_NAME COM)
target_compile_options(swiftCOM PRIVATE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature BuiltinModule>")
target_link_libraries(swiftCOM PRIVATE
  swiftCore
  swiftSynchronization
  $<$<PLATFORM_ID:Windows>:swiftWinSDK>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swiftCOM
  EXPORT SwiftSupplementalTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftCOM)
install_swift_interface(swiftCOM)

embed_manifest(swiftCOM)
