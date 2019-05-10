import TestsUtils
public let CSVParsing = [
  BenchmarkInfo(
    name: "CSVParsing.Char",
    runFunction: run_CSVParsing_characters,
    tags: [.miniapplication, .api, .String],
    setUpFunction: { buildWorkload() }),
  BenchmarkInfo(
    name: "CSVParsing.Scalar",
    runFunction: run_CSVParsing_scalars,
    tags: [.miniapplication, .api, .String],
    setUpFunction: { buildWorkload() }),
  BenchmarkInfo(
    name: "CSVParsing.UTF16",
    runFunction: run_CSVParsing_utf16,
    tags: [.miniapplication, .api, .String],
    setUpFunction: { buildWorkload() }),
  BenchmarkInfo(
    name: "CSVParsing.UTF8",
    runFunction: run_CSVParsing_utf8,
    tags: [.miniapplication, .api, .String],
    setUpFunction: { buildWorkload() }),
  BenchmarkInfo(
    name: "CSVParsingAlt2",
    runFunction: run_CSVParsingAlt,
    tags: [.miniapplication, .api, .String],
    setUpFunction: { buildWorkload() },
    legacyFactor: 11),
  BenchmarkInfo(
    name: "CSVParsingAltIndices2",
    runFunction: run_CSVParsingAltIndices,
    tags: [.miniapplication, .api, .String],
    setUpFunction: { buildWorkload() },
    legacyFactor: 11),
]

// Convenience
extension Collection where Self == Self.SubSequence {
  mutating func remove(upTo index: Index) {
    self = suffix(from: index)
  }

  mutating func remove(upToAndIncluding index: Index) {
    self = suffix(from: self.index(after: index))
  }
}

struct ParseError: Error {
  var message: String
}

protocol StringView: Collection where Element: Equatable, SubSequence == Self {
  init()
  mutating func append(_ other: Self)
}
extension StringView {
  mutating func removeAll() { self = Self() }
}
extension StringView where Self: RangeReplaceableCollection {
  mutating func append(_ other: Self) {
    self += other
  }
}

extension Substring: StringView {}
extension Substring.UnicodeScalarView: StringView {}
extension Substring.UTF8View: StringView {
  init() { self = Substring().utf8 }

  // We're hoping for only whole-scalar operations
  mutating func append(_ other: Substring.UTF8View) {
    self = (Substring(self) + Substring(other)).utf8
  }
}
extension Substring.UTF16View: StringView {
  init() { self = Substring().utf16 }

  // We're hoping for only whole-scalar operations
  mutating func append(_ other: Substring.UTF16View) {
    self = (Substring(self) + Substring(other)).utf16
  }
}

let comma = ",".utf16.first!
let newline = "\n".utf16.first!
let carriageReturn = "\n".utf16.first!
let quote = "\"".utf16.first!

@inline(__always) // ... and always specialize
func parseCSV<View: StringView, State>(
  _ remainder: inout View,
  initialState: State,
  quote: View.Element,
  comma: View.Element,
  newline: View.Element,
  carriageReturn: View.Element,
  processField: (inout State, Int, View) -> ()
) throws -> State {
  // Helper to parse a quoted field
  @inline(__always) // ... and always specialize
  func parseQuotedField(_ remainder: inout View) throws -> View {
    var result: View = View()

    while !remainder.isEmpty {
      guard let nextQuoteIndex = remainder.firstIndex(of: quote) else {
        throw ParseError(message: "Expected a closing \"")
      }

      // Append until the next quote
      result.append(remainder[nextQuoteIndex...nextQuoteIndex])
      result.append(remainder.prefix(upTo: nextQuoteIndex))
      remainder.remove(upToAndIncluding: nextQuoteIndex)

      guard let peek = remainder.first else {
        // End of the string
        return result
      }

      switch peek {
      case quote: // two quotes after each other is an escaped quote
        result.append(remainder[...remainder.startIndex])
        remainder.removeFirst()
      case comma: // field ending
        remainder.removeFirst()
        result.append(result[...result.startIndex])
        return result
      default:
        throw ParseError(message: "Expected closing quote to end a field")
      }
    }

    throw ParseError(message: "Expected a closing quote")
  }

  // Helper to parse a field
  @inline(__always) // ... and always specialize
  func parseField(_ remainder: inout View) throws -> View? {
    guard let start = remainder.first else { return nil }
    switch start {
    case quote:
      remainder.removeFirst() // remove the first quote
      return try parseQuotedField(&remainder)
    case newline:
      return nil
    default:
      // This is the most common case and should ideally be super fast...
      var index = remainder.startIndex
      while index < remainder.endIndex {
        switch remainder[index] {
        case comma:
          defer { remainder.remove(upToAndIncluding: index) }
          return remainder.prefix(upTo: index)
        case newline:
          let result = remainder.prefix(upTo: index)
          remainder.remove(upTo: index)
          return result
        case quote:
          throw ParseError(message: "Quotes can only surround the whole field")
        default:
          remainder.formIndex(after: &index)
        }
      }
      let result = remainder
      remainder.removeAll()
      return result
    }
  }

  // Helper to parse a line
  @inline(__always) // ... and always specialize
  func parseLine(
    _ remainder: inout View,
    result: inout State,
    processField: (inout State, Int, View) -> ()
  ) throws -> Bool {
    var fieldNumber = 0

    while let field = try parseField(&remainder) {
      processField(&result, fieldNumber, field)
      fieldNumber += 1
    }

    if !remainder.isEmpty {
      let next = remainder[remainder.startIndex]
      guard next == carriageReturn || next == newline else {
        throw ParseError(message: "Expected a newline or CR, got \(next)")
      }

     while let x = remainder.first, x == carriageReturn || x == newline {
        remainder.removeFirst()
      }
    }

    return !remainder.isEmpty && fieldNumber > 0
  }

  var state = initialState
  while try parseLine(
    &remainder, result: &state, processField: processField
  ) {}
  return state
}

// More concrete convenience
@inline(__always) // ... and always specialize
func parseCSV<State>(
  _ remainder: inout Substring,
  initialState: State,
  processField: (inout State, Int, Substring) -> ()
) throws -> State {
  return try parseCSV(
    &remainder,
    initialState: initialState,
    quote: "\"",
    comma: ",",
    newline: "\n",
    carriageReturn: "\r\n",
    processField: processField)
}
@inline(__always) // ... and always specialize
func parseCSV<State>(
  _ remainder: inout Substring.UnicodeScalarView,
  initialState: State,
  processField: (inout State, Int, Substring.UnicodeScalarView) -> ()
) throws -> State {
  return try parseCSV(
    &remainder,
    initialState: initialState,
    quote: "\"".unicodeScalars.first!,
    comma: ",".unicodeScalars.first!,
    newline: "\n".unicodeScalars.first!,
    carriageReturn: "\r\n".unicodeScalars.first!,
    processField: processField)
}
@inline(__always) // ... and always specialize
func parseCSV<State>(
  _ remainder: inout Substring.UTF16View,
  initialState: State,
  processField: (inout State, Int, Substring.UTF16View) -> ()
) throws -> State {
  return try parseCSV(
    &remainder,
    initialState: initialState,
    quote: "\"".utf16.first!,
    comma: ",".utf16.first!,
    newline: "\n".utf16.first!,
    carriageReturn: "\r\n".utf16.first!,
    processField: processField)
}
@inline(__always) // ... and always specialize
func parseCSV<State>(
  _ remainder: inout Substring.UTF8View,
  initialState: State,
  processField: (inout State, Int, Substring.UTF8View) -> ()
) throws -> State {
  return try parseCSV(
    &remainder,
    initialState: initialState,
    quote: "\"".utf8.first!,
    comma: ",".utf8.first!,
    newline: "\n".utf8.first!,
    carriageReturn: "\r\n".utf8.first!,
    processField: processField)
}

extension String {
  func parseAlt() -> [[String]] {
    var result: [[String]] = [[]]
    var currentField = "".unicodeScalars
    var inQuotes = false
    func flush() {
      result[result.endIndex-1].append(String(currentField))
      currentField.removeAll()
    }
    for c in self.unicodeScalars {
      switch (c, inQuotes) {
      case (",", false):
        flush()
      case ("\n", false):
        flush()
        result.append([])
      case ("\"", _):
        inQuotes = !inQuotes
        currentField.append(c)
      default:
        currentField.append(c)
      }
    }
    flush()
    return result
  }
  func parseAltIndices() -> [[Substring]] {
    var result: [[Substring]] = [[]]
    var fieldStart = self.startIndex
    var inQuotes = false
    func flush(endingAt end: Index) {
      result[result.endIndex-1].append(self[fieldStart..<end])
    }
    for i in self.unicodeScalars.indices {
      switch (self.unicodeScalars[i], inQuotes) {
      case (",", false):
        flush(endingAt: i)
        fieldStart = self.unicodeScalars.index(after: i)
      case ("\n", false):
        flush(endingAt: i)
        fieldStart = self.unicodeScalars.index(after: i)
        result.append([])
      case ("\"", _):
        inQuotes = !inQuotes
      default:
        continue
      }
    }
    flush(endingAt: endIndex)
    return result
  }
}

let workloadBase = """
  Heading1,Heading2,Heading3,Heading4,Heading5,Heading6,Heading7
  FirstEntry,"secondentry",third,fourth,fifth,sixth,seventh
  zéro,un,deux,trois,quatre,cinq,six
  pagh,wa',cha',wej,IoS,vagh,jav
  ᬦᬸᬮ᭄,ᬲᬶᬓᬶ,ᬤᬸᬯ,ᬢᭂᬮᬸ,ᬧᬧᬢ᭄,ᬮᬶᬫᬾ,ᬦᭂᬦᭂᬫ᭄
  unu,du,tri,kvar,kvin,ses,sep
  "quoted","f""ield","with a comma ',' in it","and some \n for good measure", five, six, seven
  𐌏𐌉𐌍𐌏,𐌃𐌏,𐌕𐌓𐌉,𐌐𐌄𐌕𐌏𐌓,𐌐𐌄𐌌𐌐𐌄,𐌔𐌖𐌄𐌊𐌏𐌔,𐌔𐌄𐌗𐌕𐌀𐌌
  zero,un,duo.tres.quantro,cinque,sex
  nolla,yksi,kaksi,kolme,neljä,viisi,kuusi
  really long field, because otherwise, small string opt,imizations may trivial,ize the copies that, were trying to also, measure here!!!!
  нула,једин,два,три,четыри,петь,шесть
  一,二,三,四,五,六,七
  saquui,ta'lo,tso'i,nvgi,hisgi,sudali,galiquogi

  """

let targetRowNumber = 50
let repeatCount = targetRowNumber / workloadBase.split(separator: "\n").count
let workload = String(
  repeatElement(workloadBase, count: repeatCount).joined().dropLast())

public func buildWorkload() {
  let contents = workload
  // Validate that all the parsers produce the same results.
  let alt: [[String]] = contents.parseAlt()
  let altIndices: [[String]] = contents.parseAltIndices().map {
    $0.map { String($0) }
  }
  CheckResults(alt.elementsEqual(altIndices))

  var remainder = workload[...]

  let parseResult: [[String]] = try! parseCSV(&remainder, initialState: []) {
    (res: inout [[String]], num, substr) in
    let field = String(substr)
    if num == 0 {
      res.append([field])
    } else {
      res[res.endIndex-1].append(field)
    }
  }
  CheckResults(alt.elementsEqual(parseResult))
}

@inline(never)
public func run_CSVParsing_characters(_ N: Int) {
  let contents = workload
  for _ in 0..<N {
    var remainder = contents[...]
    let result = try! parseCSV(&remainder, initialState: 0) {
      (result: inout Int, _, substr) in
      result += 1
      blackHole(substr)
    }
    blackHole(result)
  }
}

@inline(never)
public func run_CSVParsing_scalars(_ N: Int) {
  let contents = workload.unicodeScalars
  for _ in 0..<N {
    var remainder = contents[...]
    let result = try! parseCSV(&remainder, initialState: 0) {
      (result: inout Int, _, substr) in
      result += 1
      blackHole(substr)
    }
    blackHole(result)
  }
}

@inline(never)
public func run_CSVParsing_utf16(_ N: Int) {
  let contents = workload.utf16
  for _ in 0..<N {
    var remainder = contents[...]
    let result = try! parseCSV(&remainder, initialState: 0) {
      (result: inout Int, _, substr) in
      result += 1
      blackHole(substr)
    }
    blackHole(result)
  }
}

@inline(never)
public func run_CSVParsing_utf8(_ N: Int) {
  let contents = workload.utf8
  for _ in 0..<N {
    var remainder = contents[...]
    let result = try! parseCSV(&remainder, initialState: 0) {
      (result: inout Int, _, substr) in
      result += 1
      blackHole(substr)
    }
    blackHole(result)
  }
}


@inline(never)
public func run_CSVParsingAlt(_ N: Int) {
  let contents = workload
  for _ in 0..<N {
    blackHole(contents.parseAlt())
  }
}

@inline(never)
public func run_CSVParsingAltIndices(_ N: Int) {
  let contents = workload
  for _ in 0..<N {
    blackHole(contents.parseAltIndices())
  }
}

