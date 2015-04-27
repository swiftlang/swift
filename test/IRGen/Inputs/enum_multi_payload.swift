
enum Either<T, U> {
  case Left(T)
  case Right(U)
}

enum EitherOr<T, U> {
  case Left(T)
  case Middle
  case Center
  case Right(U)
}

class C {}

