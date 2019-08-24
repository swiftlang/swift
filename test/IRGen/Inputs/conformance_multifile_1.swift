protocol Super {}
protocol P : Super { }

enum E {}

extension E : P { }

enum E2 {
  case Filter(P)
}
