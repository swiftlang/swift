// RUN: %target-swift-frontend -enable-sil-opaque-values -parse-as-library -emit-sil -O -enable-library-evolution %s | %FileCheck %s

public struct Gadget {
// Verify the default accessor's arguments.  When AccessPathVerification runs,
// it will be checked that the ParamDecl that AddressLowering synthesizes has a
// non-null decl context.
// CHECK-LABEL: sil @$s25opaque_values_O_resilient6GadgetV5whichA2C5WhichO_tcfC : {{.*}} {
// CHECK:       bb0(%0 : $*Gadget, %1 : $*Gadget.Which, %2 : $@thin Gadget.Type):
// CHECK-LABEL: } // end sil function '$s25opaque_values_O_resilient6GadgetV5whichA2C5WhichO_tcfC'
  public init(which: Which = .that) {
    fatalError()
  }

  public enum Which {
    case that
    case this(() -> Gadget)
    case theOther
  }
}
