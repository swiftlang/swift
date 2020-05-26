// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing -enable-private-imports %s
func funcWithDefaultArg(_ arg : String = String("hello")) {
}
