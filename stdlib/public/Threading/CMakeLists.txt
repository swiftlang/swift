add_swift_target_library(swiftThreading OBJECT_LIBRARY
  "${SWIFT_SOURCE_DIR}/lib/Threading/C11.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Threading/Linux.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Threading/Nothreads.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Threading/Pthreads.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Threading/Win32.cpp"
  INSTALL_IN_COMPONENT never_install)
