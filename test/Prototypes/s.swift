import Swift

import Dispatch
import Darwin

let args = CommandLine.arguments.dropFirst()
let testOld = !args.contains { $0.hasPrefix("--") } || args.contains("--old")
let testNew = !args.contains { $0.hasPrefix("--") } || args.contains("--new")
let testFilter = args.filter { !$0.hasPrefix("--") }

func timer(_ _caller : String = #function, _ body: ()->()) {
  if !testOld && _caller.hasSuffix("_old()") { return }
  if !testNew && _caller.hasSuffix("_new()") { return }
  
  if !testFilter.isEmpty
  && !testFilter.contains(where: { _caller.hasPrefix($0) }) { return  }
  
  var tmin = Double.infinity
  var tmax = 0.0, sum = 0.0
  var reps = testFilter.isEmpty ? 3 : 5
  _sanityCheck({ reps=1; return true }())
  
  for _ in 0..<reps {
    let start = DispatchTime.now()
    body()
    let end = DispatchTime.now()
    let milliseconds = (Double(end.uptimeNanoseconds) - Double(start.uptimeNanoseconds)) / 1_000_000.0
    tmin = min(tmin, milliseconds)
    sum += milliseconds
    tmax = max(tmax, milliseconds)
  }
  var prefix = "\(_caller),\(String(repeating: " ", count: 50 - _caller.utf16.count))\(Int(tmin)),"
  let spaces = String(repeating: " ", count: 60 - prefix.utf16.count)
  print(prefix, spaces, "±\(Int((sum / Double(reps) - tmin) / 2))")
}

test_newStringCore(instrumentedWith: timer)

