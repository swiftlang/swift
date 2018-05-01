// RUN: %target-typecheck-verify-swift

let a = [10, 20, 30, 40, 50, 60]

_ = a.index(of: 30)
_ = a.firstIndex(of: 30)
_ = a.index(where: { $0 > 30 })
_ = a.firstIndex(where: { $0 > 30 })
