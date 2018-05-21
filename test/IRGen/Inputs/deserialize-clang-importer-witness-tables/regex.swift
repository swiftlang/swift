
import Foundation

public struct RegEx {
    public let pattern: String
    fileprivate let regex: NSRegularExpression
    public typealias Options = NSRegularExpression.Options
    
    public init(pattern: String, options: Options = []) throws {
        self.pattern = pattern
        self.regex = try NSRegularExpression(pattern: pattern, options: options)
    }
    
    /// Returns a match group for the first match, or nil if there was no match.
    public func firstMatch(in string: String) -> [String]? {
        let nsString = string as NSString
        
        return regex.firstMatch(in: string, range: NSMakeRange(0, nsString.length)).map { match -> [String] in
            return (1 ..< match.numberOfRanges).map { idx -> String in
                let range = match.range(at: idx)
                return range.location == NSNotFound ? "" : nsString.substring(with: range)
            }
        }
    }
}