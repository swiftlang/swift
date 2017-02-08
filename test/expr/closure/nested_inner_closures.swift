// RUN: %target-typecheck-verify-swift

assert({ () -> Bool in
  return true
}(), "")

var x = ({ () -> String in return "s" })()
