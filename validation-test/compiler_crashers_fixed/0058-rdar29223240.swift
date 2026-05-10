// RUN: %target-swift-frontend %s -emit-ir

extension Array where Iterator.Element == UTF8.CodeUnit {
  var u8str : String {
    return ""
  }
}
