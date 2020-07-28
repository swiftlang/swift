// RUN: %target-swift-frontend %s -emit-ir

extension Array where Iterator.Element == Unicode.UTF8.CodeUnit {
  var u8str : String {
    return ""
  }
}
