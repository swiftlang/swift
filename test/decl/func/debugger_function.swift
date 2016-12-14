// RUN: %target-typecheck-verify-swift -enable-throw-without-try -debugger-support

func foo() throws -> Int { return 0 }

@LLDBDebuggerFunction
func test0() throws -> Int {
  var x: Int = 0
  x = foo()
  x = try foo()
  return x
}

@LLDBDebuggerFunction
func test1() -> Int {
  do {
    var x: Int = 0
    x = foo()
    x = try foo()
    return x
  } catch {
    return 0
  }
}

@LLDBDebuggerFunction
func test2() -> Int {
  do {
    do {
      var x: Int = 0
      x = foo() // expected-error {{call can throw but is not marked with 'try'}}
      x = try foo()
      return x
    } catch {
      return 0
    }
  } catch {
    return 0
  }
}

@LLDBDebuggerFunction
func test3() {
  do {
  } catch {
  }
}

@LLDBDebuggerFunction
func test4() {
  do {
    do {}
    catch {} // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
  } catch {
  }
}
