// RUN: %target-swiftc_driver -O -Rpass-missed=sil-opt-remark-gen -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil %s -o /dev/null -Xfrontend -verify -Xllvm -optremarkgen-visit-implicit-autogen-funcs=1

// From the constructor.
class Klass {} // expected-remark {{heap allocated ref of type 'Klass'}}

struct KlassPair {
    var lhs: Klass // expected-remark {{retain of type 'Klass'}}
                   // expected-note @-1 {{of 'self.lhs'}}
                   // expected-remark @-2 {{release of type 'Klass'}}
                   // expected-note @-3 {{of 'self.lhs'}}
    var rhs: Klass // expected-remark {{retain of type 'Klass'}}
                   // expected-note @-1 {{of 'self.rhs'}}
                   // expected-remark @-2 {{release of type 'Klass'}}
                   // expected-note @-3 {{of 'self.rhs'}}
}

