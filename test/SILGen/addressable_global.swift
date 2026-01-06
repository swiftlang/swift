// RUN: %target-swift-emit-silgen -parse-as-library -disable-availability-checking %s | %FileCheck %s

enum Color {
    case R, G, B
}

let colors: InlineArray<_, Color> = [
    .G, .G, .R, .G, .G, .B,
    .G, .G, .B, .G, .G, .R,
    .B, .R, .G, .R, .B, .G,
    .G, .G, .B, .G, .G, .R,
    .G, .G, .R, .G, .G, .B,
    .R, .B, .G, .B, .R, .G,
]

// CHECK-LABEL: sil{{.*}} @$s{{.*}}4main
func main() {
    // CHECK: [[POINTER:%.*]] = apply {{.*}}() : $@convention(thin) () -> Builtin.RawPointer
    // CHECK: [[ADDRESS:%.*]] = pointer_to_address [[POINTER]]
    // CHECK: [[GET_SPAN:%.*]] = function_ref @$s{{.*}}11InlineArray{{.*}}4span
    // CHECK: apply [[GET_SPAN]]<{{.*}}>([[ADDRESS]])
    let span = colors.span
    for elem in [3, 9, 8] {
        switch span[elem] {
        case .R: print(0)
        case .G: print(1)
        case .B: print(2)
        }
    }
}
