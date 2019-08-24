import Foundation

@inline(__always)
public func replace(rgx: NSRegularExpression, in: String, with: String, x: NSRange) -> String {
  return rgx.stringByReplacingMatches(in: `in`, options: [], range: x, withTemplate: with)
}
