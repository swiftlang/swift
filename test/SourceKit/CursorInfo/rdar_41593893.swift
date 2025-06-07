// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %sourcekitd-test -req=cursor -pos=8:37 %t/first.swift -- %t/first.swift %t/second.swift | %FileCheck %s

// CHECK: source.lang.swift.ref.var.instance (6:9-6:12)

//--- first.swift
protocol ChatDataSourceDelegateProtocol {
    func chatDataSourceDidUpdate()
}

class BaseChatViewController {
    var foo = 1
    func bar() {
      print(self . /*cursor-info->*/foo)
    }
}

//--- second.swift
extension BaseChatViewController: ChatDataSourceDelegateProtocol {
    func chatDataSourceDidUpdate() { fatalError() }
}
