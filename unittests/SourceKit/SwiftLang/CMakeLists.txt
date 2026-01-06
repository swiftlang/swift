if(NOT SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_EMBEDDED_VARIANTS}")
  add_swift_unittest(SourceKitSwiftLangTests
    CursorInfoTest.cpp
    CloseTest.cpp
    EditingTest.cpp
    )
  target_link_libraries(SourceKitSwiftLangTests PRIVATE SourceKitSwiftLang)
  target_compile_definitions(SourceKitSwiftLangTests PRIVATE
    SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
endif()
