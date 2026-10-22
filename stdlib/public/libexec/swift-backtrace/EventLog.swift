#if os(Windows)

import CRT
import WinSDK

// ReportEventW() limits each insertion string to 31,839 UTF-16 code units,
// so the message holds as much of the crash log as fits, and the whole log
// is attached as UTF-8 event data.
private let maxMessageLength = 31_839

private let truncationNote =
  "\r\n...\r\n(truncated; the full crash log is attached as event data)"

/// Returns as many whole lines of `log` as fit in an event message.
private func truncatedToFitIfNeeded(_ log: String) -> String {
  if log.utf16.count <= maxMessageLength {
    return log
  }
  let limit = log.utf16.index(
    log.startIndex, offsetBy: maxMessageLength - truncationNote.utf16.count)
  let end = log[..<limit].lastIndex(of: "\r\n") ?? log.startIndex
  return String(log[..<end]) + truncationNote
}

/// Write `text` to the Application event log as a single error event.
/// Returns true if the event was logged.
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

  let log = text.split(separator: "\n", omittingEmptySubsequences: false)
    .joined(separator: "\r\n")
  var data = Array(log.utf8)

  return truncatedToFitIfNeeded(log).withCString(encodedAs: UTF16.self) {
    var string: LPCWSTR? = $0
    return ReportEventW(hEventLog, EVENTLOG_ERROR_TYPE, 0, 1, nil,
                        1, DWORD(data.count), &string, &data)
  }
}

#endif
