add_swift_target_executable(swift-backtrace BUILD_WITH_STDLIB
  main.swift
  AnsiColor.swift
  Target.swift
  Themes.swift
  Utils.swift

  SWIFT_MODULE_DEPENDS
    _Backtracing

  INSTALL_IN_COMPONENT stdlib
  COMPILE_FLAGS -parse-as-library)
