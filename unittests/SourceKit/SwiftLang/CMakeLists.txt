if(NOT SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_EMBEDDED_VARIANTS}")

  add_swift_unittest(SourceKitSwiftLangTests
    CursorInfoTest.cpp
    EditingTest.cpp
    )

  target_link_libraries(SourceKitSwiftLangTests
    PRIVATE
    SourceKitSwiftLang
    )

  set_property(TARGET SourceKitSwiftLangTests APPEND_STRING PROPERTY COMPILE_FLAGS
    " '-DSWIFTLIB_DIR=\"${SWIFTLIB_DIR}\"'")

endif()
