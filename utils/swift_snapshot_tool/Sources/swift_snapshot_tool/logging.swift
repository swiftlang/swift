import Foundation

func log(_ msg: String) {
  let msgWithSpace = "\(msg)\n"
  msgWithSpace.data(using: .utf8)
    .map(FileHandle.standardError.write)
}
