// TODO: Relies on AnyObject lvalues <rdar://problem/17755906>
// R/UN: %target-run-simple-swift | FileCheck %s
// RUN: not %target-build-swift -parse

import Foundation

class Model : NSObject {
  let undoManager: NSUndoManager = { 
    let u = NSUndoManager() 
    u.groupsByEvent = false
    return u
  }()

  var _value: Float = 0
  var value: Float {
    get {
      return _value
    }
    set(newValue) {
      undoManager.prepareWithInvocationTarget(self).value = _value
      _value = newValue
    }
  }

  func print(message: String) {
    print(message + ": \(value)")
  }
}


let m = Model()
let u = m.undoManager

m.print("start")
// CHECK: start: 0

u.beginUndoGrouping()
m.value = 1
u.endUndoGrouping()
m.print("set")
// CHECK: set: 1

u.undo()
m.print("undo")
// CHECK: undo: 0

u.redo()
m.print("redo")
// CHECK: redo: 1

u.beginUndoGrouping()
m.value = 1.5
m.value = 2
u.endUndoGrouping()
m.print("set 2")
// CHECK: set 2: 2
u.beginUndoGrouping()
m.value = 2.5
m.value = 3
u.endUndoGrouping()
m.print("set 3")
// CHECK: set 3: 3
u.beginUndoGrouping()
m.value = 3.5
m.value = 4
u.endUndoGrouping()
m.print("set 4")
// CHECK: set 4: 4

u.undo()
m.print("undo 4")
// CHECK: undo 4: 3
u.undo()
m.print("undo 3")
// CHECK: undo 3: 2
u.undo()
m.print("undo 2")
// CHECK: undo 2: 1
u.undo()
m.print("undo 1")
// CHECK: undo 1: 0

u.redo()
m.print("redo 1")
// CHECK: redo 1: 1
u.redo()
m.print("redo 2")
// CHECK: redo 2: 2
u.redo()
m.print("redo 3")
// CHECK: redo 3: 3
u.redo()
m.print("redo 4")
// CHECK: redo 4: 4

