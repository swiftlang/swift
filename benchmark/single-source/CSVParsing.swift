struct ParseError: Error {
    var message: String
}

let comma = ",".utf16.first!
let newline = "\n".utf16.first!
let carriageReturn = "\n".utf16.first!
let quote = "\"".utf16.first!

@inline(__always) func parseQuotedField(_ remainder: inout Substring) throws -> Substring? {
    var result: Substring = "" // we accumulate the result
    
    while !remainder.isEmpty {
        guard let nextQuoteIndex = remainder.index(of: "\"") else {
            throw ParseError(message: "Expected a closing \"")
        }
        
        // Append until the next quote
        result += remainder.prefix(upTo: nextQuoteIndex)
        remainder.remove(upToAndIncluding: nextQuoteIndex)
        
        if let peek = remainder.utf16.first {
            switch peek {
            case quote: // two quotes after each other is an escaped quote
                remainder.removeFirst()
                result.append("\"")
            case comma: // field ending
                remainder.removeFirst()
                return result
            default:
                return result
            }
        } else {
            // End of the string
            return result
        }
    }
    
    throw ParseError(message: "Expected a closing quote")
}

// Consume a single field from `remainder`
@inline(__always) func parseField(_ remainder: inout Substring) throws -> Substring? {
    guard let start = remainder.utf16.first else { return nil }
    switch start {
    case quote:
        remainder.removeFirst() // remove the first quote
        return try parseQuotedField(&remainder)
    case newline:
        return nil
    default:
        // This is the most common case and should ideally be super fast...
        var index = remainder.utf16.startIndex
        while index < remainder.utf16.endIndex {
            switch remainder.utf16[index] {
            case comma:
                defer { remainder.remove(upToAndIncluding: index) }
                return remainder.prefix(upTo: index)
            case newline:
                let result = remainder.prefix(upTo: index)
                remainder.remove(upTo: index)
                return result
            default:
                remainder.utf16.formIndex(after: &index)
            }
        }
        let result = remainder
        remainder.removeAll()
        return result
    }
}

extension Substring {
    mutating func remove(upTo index: Index) {
        self = suffix(from: index)
    }
    
    mutating func remove(upToAndIncluding index: Index) {
        self = suffix(from: self.index(after: index))
    }
}

// Consume a single line from `remainder`
func parseLine<State>(_ remainder: inout Substring, result: inout State, processField: (inout State, Int, Substring) -> ()) throws -> Bool {
    var fieldNumber = 0
    
    while let field = try parseField(&remainder) {
        processField(&result, fieldNumber, field)
        fieldNumber += 1
    }
    
    if !remainder.isEmpty {
        let next = remainder.utf16[remainder.utf16.startIndex]
        guard next == carriageReturn || next == newline else {
            throw ParseError(message: "Expected a newline or CR, got \(next)")
        }
        
        while let x = remainder.utf16.first, x == carriageReturn || x == newline {
            remainder.utf16.removeFirst()
        }
    }
    
    return !remainder.isEmpty && fieldNumber > 0
}

import Foundation

func time<Result>(name: StaticString = #function, line: Int = #line, _ f: () throws -> Result) rethrows -> Result {
    let startTime = DispatchTime.now()
    let result = try f()
    let endTime = DispatchTime.now()
    let diff = Double(endTime.uptimeNanoseconds - startTime.uptimeNanoseconds) / 1_000_000_000 as Double
    print("\(name) (line \(line)): \(diff) sec")
    return result
}

try time {
    // URL for this file: http://www.maxmind.com/download/worldcities/worldcitiespop.txt.gz
    //    let file = URL(fileURLWithPath: "/Users/chris/Downloads/1489325/stops.txt")
    let file = URL(fileURLWithPath: "worldcitiespop.txt")
    
    // The + "\n" is a a trick by Ole Begemann, which forces the String to be a Swift String (not an NSString). It makes it more than twice as fast on my computer...
    let contents = try String(contentsOf: file, encoding: .isoLatin1) + ""
    
    var remainder = contents[...]
    
    var result: Int = 0
    var x: () = ()
    
    while !remainder.isEmpty {
        _ = try parseLine(&remainder, result: &x, processField: { state, _, field in
            ()
        })
        if result < 10 {
            print("result: \(result)")
        }
        result += 1
        if result % 100_000 == 0 {
            print(result)
        }
    }
    print(result)
    
}
