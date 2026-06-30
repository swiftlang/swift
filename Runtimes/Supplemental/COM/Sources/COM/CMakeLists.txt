add_library(swift_COM
  CLSID.swift
  COMActivationOptions.swift
  COMAggregate.swift
  COMContext.swift
  COMError.swift
  COMInterface.swift
  COMInterfaceResolver.swift
  COMServerInfo.swift
  GUID.swift
  HRESULT.swift
  IErrorInfo.swift
  IID.swift
  ISwiftObject.swift
  IUnknown.swift
  SwiftRuntime.swift
  TearOffInterfaces.swift
  ThreadingModel.swift
  UnsafeMutableRawPointer+COM.swift)
set_target_properties(swift_COM PROPERTIES
  Swift_MODULE_NAME COM)
target_link_libraries(swift_COM PRIVATE
  swiftCore
  swiftSynchronization
  $<$<PLATFORM_ID:Windows>:swiftWinSDK>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swift_COM
  EXPORT SwiftSupplementalTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")

embed_manifest(swift_COM)
