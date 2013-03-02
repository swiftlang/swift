configure_file(
  Swift.xcplugin/Contents/Resources/Swift.xcspec.in
  Swift.xcplugin/Contents/Resources/Swift.xcspec
  @ONLY)

set(XCODE_PLUGIN_DIR "/Library/Application Support/Developer/Shared/Xcode/Plug-ins")
install(
  FILES
    Swift.xcplugin/Contents/Info.plist
    Swift.xcplugin/Contents/version.plist
  DESTINATION "${XCODE_PLUGIN_DIR}/Swift.xcplugin/Contents")

install(
  FILES
    Swift.xcplugin/Contents/Resources/Swift.pbfilespec
    ${CMAKE_CURRENT_BINARY_DIR}/Swift.xcplugin/Contents/Resources/Swift.xcspec
  DESTINATION "${XCODE_PLUGIN_DIR}/Swift.xcplugin/Contents/Resources")
