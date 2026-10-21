#if os(Windows)

import CRT
import WinSDK

// ReportEventW() limits each insertion string to 31,839 UTF-16 code units.
// Crash logs (which default to the full preset here) often exceed that, so
// they are split into numbered parts, each sent as its own event.
private let maxMessageLength = 31_839
private let maxHeaderLength = 16 // room for "(i/n)\r\n"

private extension Substring {
  /// Splits into pieces of at most `maxLength` UTF-16 code units without
  /// breaking a Character apart. Returns each piece with its length.
  func pieces(maxLength: Int) -> [(Substring, Int)] {
    let total = utf16.count
    guard total > maxLength else {
      return [(self, total)]
    }

    var pieces: [(Substring, Int)] = []
    var start = startIndex
    var length = 0
    for i in indices {
      let n = self[i].utf16.count
      if length > 0, length + n > maxLength {
        pieces.append((self[start..<i], length))
        start = i
        length = 0
      }
      length += n
    }
    pieces.append((self[start...], length))
    return pieces
  }
}

private extension String {
  /// Splits on "\n", "\r", or "\r\n", scanning bytes to avoid grapheme breaking.
  func splitLines() -> [Substring] {
    let cr = UInt8(ascii: "\r"), lf = UInt8(ascii: "\n")

    var lines: [Substring] = []
    var rest = self[...]
    while let i = rest.utf8.firstIndex(where: { $0 == cr || $0 == lf }) {
      lines.append(rest[..<i])
      let isCRLF = rest.utf8[i...].starts(with: [cr, lf])
      rest = rest[rest.utf8.index(i, offsetBy: isCRLF ? 2 : 1)...]
    }
    lines.append(rest)
    return lines
  }

  /// Splits into chunks of at most `maxLength` UTF-16 code units, joining
  /// lines with "\r\n". Lines longer than `maxLength` are split across chunks.
  func chunked(maxLength: Int) -> [String] {
    var chunks: [String] = []
    var current: [Substring] = []
    var length = 0

    for line in splitLines() {
      for (piece, n) in line.pieces(maxLength: maxLength) {
        if !current.isEmpty, length + 2 + n <= maxLength {
          current.append(piece)
          length += 2 + n
        } else {
          if !current.isEmpty {
            chunks.append(current.joined(separator: "\r\n"))
          }
          current = [piece]
          length = n
        }
      }
    }

    if !current.isEmpty {
      chunks.append(current.joined(separator: "\r\n"))
    }
    return chunks
  }
}

/// Splits `text` into messages that each fit in a single event,
/// numbering them "(i/n)" when more than one is needed.
private func messages(for text: String) -> [String] {
  let parts = text.chunked(maxLength: maxMessageLength - maxHeaderLength)
  guard parts.count > 1 else { return parts }
  return parts.enumerated().map {
    "(\($0.offset + 1)/\(parts.count))\r\n\($0.element)"
  }
}

private func report(_ message: String, to hEventLog: HANDLE) -> Bool {
  return message.withCString(encodedAs: UTF16.self) { pwszMessage in
    var string: LPCWSTR? = pwszMessage
    return ReportEventW(hEventLog, EVENTLOG_ERROR_TYPE, 0, 1, nil,
                        1, 0, &string, nil)
  }
}

/// Write `text` to the Application event log as one or more error events.
/// Returns true if every part was logged.
internal func reportToEventLog(_ text: String, source: String) -> Bool {
  let hEventLog = source.withCString(encodedAs: UTF16.self) {
    RegisterEventSourceW(nil, $0)
  }

  guard let hEventLog else {
    return false
  }

  defer {
    _ = DeregisterEventSource(hEventLog)
  }

  return messages(for: text)
    .map { report($0, to: hEventLog) }
    .allSatisfy { $0 }
}

#endif
