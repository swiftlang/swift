// FIXME: Function types don't work yet as generic parameters
struct REPLExitHandler {
  var f : () -> ()
}

var replExitHandlers : Vector<REPLExitHandler> = Vector<REPLExitHandler>()

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
