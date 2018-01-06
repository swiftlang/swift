import TestsUtils
public let CSVParsing = BenchmarkInfo(
    name: "CSVParsing",
    runFunction: run_CSVParsing,
    tags: [.miniapplication, .api, .String])

struct ParseError: Error {
    var message: String
}

let comma = ",".utf16.first!
let newline = "\n".utf16.first!
let carriageReturn = "\n".utf16.first!
let quote = "\"".utf16.first!

func parseQuotedField(_ remainder: inout Substring) throws -> Substring? {
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
func parseField(_ remainder: inout Substring) throws -> Substring? {
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

let workloadBase = """
    Heading1,Heading2,Heading3,Heading4,Heading5,Heading6,Heading7
    FirstEntry,"secondentry",third,fourth,fifth,sixth,seventh
    zéro,un,deux,trois,quatre,cinq,six
    pagh,wa',cha',wej,IoS,vagh,jav
    ᬦᬸᬮ᭄,ᬲᬶᬓᬶ,ᬤᬸᬯ,ᬢᭂᬮᬸ,ᬧᬧᬢ᭄,ᬮᬶᬫᬾ,ᬦᭂᬦᭂᬫ᭄
    unu,du,tri,kvar,kvin,ses,sep
    𐌏𐌉𐌍𐌏,𐌃𐌏,𐌕𐌓𐌉,𐌐𐌄𐌕𐌏𐌓,𐌐𐌄𐌌𐌐𐌄,𐌔𐌖𐌄𐌊𐌏𐌔,𐌔𐌄𐌗𐌕𐌀𐌌
    zero,un,duo.tres.quantro,cinque,sex
    nolla,yksi,kaksi,kolme,neljä,viisi,kuusi
    нула,једин,два,три,четыри,петь,шесть
    一,二,三,四,五,六,七,
    saquui,ta'lo,tso'i,nvgi,hisgi,sudali,galiquogi
    """
let targetRowNumber = 300_000
let repeatCount = targetRowNumber / workloadBase.split(separator: "\n").count
let workload: String = repeatElement(workloadBase, count: repeatCount).joined()

@inline(never)
public func run_CSVParsing(_ N: Int) {
    let contents = workload

    for _ in 0..<N {
        var remainder = contents[...]
        
        var result: Int = 0
        var x: () = ()
        
        while !remainder.isEmpty {
            blackHole(try? parseLine(&remainder, result: &x, processField: { state, _, field in
                ()
            }))
            blackHole(x)
            result += 1
        }
        blackHole(result)
    }
}

