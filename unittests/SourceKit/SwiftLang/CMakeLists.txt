
add_swift_unittest(SourceKitSwiftLangTests
  CursorInfoTest.cpp
  )

target_link_libraries(SourceKitSwiftLangTests
  SourceKitSwiftLang
  )

set_property(TARGET SourceKitSwiftLangTests APPEND_STRING PROPERTY COMPILE_FLAGS
  " '-DSWIFTLIB_DIR=\"${SWIFTLIB_DIR}\"'")
