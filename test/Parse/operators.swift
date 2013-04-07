// RUN: %swift -parse %s

// Import Builtin to suppress the standard library from being loaded.
import Builtin

operator infix == {
  associativity left
  precedence 110
}

operator infix => {
  associativity right
  precedence 100
}

struct Man {}
struct TheDevil {}
struct God {}

struct Five {}
struct Six {}
struct Seven {}

struct ManIsFive {}
struct TheDevilIsSix {}
struct GodIsSeven {}

struct TheDevilIsSixThenGodIsSeven {}

func == (x:Man, y:Five) -> ManIsFive {}
func == (x:TheDevil, y:Six) -> TheDevilIsSix {}
func == (x:God, y:Seven) -> GodIsSeven {}

func => (x:TheDevilIsSix, y:GodIsSeven) -> TheDevilIsSixThenGodIsSeven {}
func => (x:ManIsFive, y:TheDevilIsSixThenGodIsSeven) {}

Man() == Five() => TheDevil() == Six() => God() == Seven()

operator postfix *!* {}
operator prefix *!* {}

struct LOOK {}
struct LOOKBang {}

func [postfix] *!* (x:LOOK) -> LOOKBang {}
func [prefix] *!* (x:LOOKBang) {}

*!*LOOK()*!*
