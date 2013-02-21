// FIXME: Function types don't work yet as generic parameters
struct REPLExitHandler {
  var f : () -> ()
}

typealias REPLExitHandlerVector = Vector<REPLExitHandler>
var replExitHandlers : REPLExitHandlerVector = new REPLExitHandlerVector()

func atREPLExit(handler:() -> ()) {
  replExitHandlers.append(REPLExitHandler(handler))
}

func replExit() {
  while replExitHandlers.length > 0 {
    var handler = replExitHandlers[replExitHandlers.length-1]
    replExitHandlers.popBack()
    handler.f()
  }
}
