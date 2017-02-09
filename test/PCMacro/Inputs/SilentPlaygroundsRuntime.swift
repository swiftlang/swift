// If you're modifying this file to add or modify function signatures, you
// should be notifying the maintainer of PlaygroundLogger and also the
// maintainer of lib/Sema/PlaygroundTransform.cpp.

struct SourceRange {
  let sl : Int
  let el : Int
  let sc : Int
  let ec : Int
  var text : String {
    return "[\(sl):\(sc)-\(el):\(ec)]"
  }
}

class LogRecord {
  let text : String

  init(api : String, object : Any, name : String, id : Int, range : SourceRange) {
    var object_description : String = ""
    print(object, terminator: "", to: &object_description)
    text = range.text + " " + api + "[" + name + "='" + object_description + "']"
  }
  init(api : String, object : Any, name : String, range : SourceRange) {
    var object_description : String = ""
    print(object, terminator: "", to: &object_description)
    text = range.text + " " + api + "[" + name + "='" + object_description + "']"
  }
  init(api : String, object: Any, range : SourceRange) {
    var object_description : String = ""
    print(object, terminator: "", to: &object_description)
    text = range.text + " " + api + "['" + object_description + "']"
  }
  init(api: String, range : SourceRange) {
    text = range.text + " " + api
  }
}

func $builtin_log<T>(_ object : T, _ name : String, _ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return LogRecord(api:"$builtin_log", object:object, name:name, range : SourceRange(sl:sl, el:el, sc:sc, ec:ec))
}

func $builtin_log_with_id<T>(_ object : T, _ name : String, _ id : Int, _ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return LogRecord(api:"$builtin_log", object:object, name:name, id:id, range : SourceRange(sl:sl, el:el, sc:sc, ec:ec))
}

func $builtin_log_scope_entry(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return LogRecord(api:"$builtin_log_scope_entry", range : SourceRange(sl:sl, el:el, sc:sc, ec:ec))
}

func $builtin_log_scope_exit(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return LogRecord(api:"$builtin_log_scope_exit", range : SourceRange(sl:sl, el:el, sc:sc, ec:ec))
}

func $builtin_postPrint(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return LogRecord(api:"$builtin_postPrint", range : SourceRange(sl:sl, el:el, sc:sc, ec:ec))
}

func $builtin_send_data(_ object:AnyObject?) {
  let would_print = ((object as! LogRecord).text)
}


