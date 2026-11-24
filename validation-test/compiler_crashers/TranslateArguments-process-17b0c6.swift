// {"kind":"emit-silgen","original":"c7ac2e2b","signature":"(anonymous namespace)::TranslateArguments::process(swift::Lowering::AbstractionPattern, swift::ArrayRefView<swift::AnyFunctionType::Param, swift::AnyFunctionType::CanParam, swift::AnyFunctionType::CanParam::getFromParam(swift::AnyFunctionType::Param const&), true>, swift::Lowering::AbstractionPattern, swift::ArrayRefView<swift::AnyFunctionType::Param, swift::AnyFunctionType::CanParam, swift::AnyFunctionType::CanParam::getFromParam(swift::AnyFunctionType::Param const&), true>, bool)","signatureAssert":"Assertion failed: (innerPackTy->getNumElements() == innerSubstParams.size()), function expandPackInnerParam","signatureNext":"buildThunkBody"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<each b, each c>(
  d: repeat each b, e: repeat each c,
  f: (repeat (each b, (each c)?)) -> Int = { (g: repeat (each b, (each c)?)) in
    0
  }
) {
  a(d: 0, "", true, e: "", 0, nil as Double?)
}
