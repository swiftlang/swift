// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// REQUIRES: objc_interop

import Foundation

class UITableView {
  var rowHeight1: CGFloat = 0.0
  var rowHeight2: CGFloat! = 0.0
}

func test() {
  let tableView = UITableView()
  let _: CGFloat = (1 + 1) * (tableView.rowHeight1 + 2) + 20 + 60
  let _: CGFloat = (1 + 1) * (tableView.rowHeight2 + 2) + 20 + 60
}
