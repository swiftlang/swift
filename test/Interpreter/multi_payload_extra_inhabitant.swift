// RUN: %empty-directory(%t)

// RUN: %target-build-swift -parse-stdlib -Xfrontend -verify-type-layout -Xfrontend SpareBitExtraInhabitants -Xfrontend -verify-type-layout -Xfrontend SpareBitSingleExtraInhabitant -Xfrontend -verify-type-layout -Xfrontend SpareBitNoExtraInhabitant -Xfrontend -verify-type-layout -Xfrontend SpareBitNoExtraInhabitant2 -Xfrontend -verify-type-layout -Xfrontend TwoTagExtraInhabitants -Xfrontend -verify-type-layout -Xfrontend ThreeTagExtraInhabitants -Xfrontend -verify-type-layout -Xfrontend NoTagExtraInhabitants -Xfrontend -verify-type-layout -Xfrontend DynamicExtraInhabitantsNever -Xfrontend -verify-type-layout -Xfrontend DynamicExtraInhabitantsZeroBytes -Xfrontend -verify-type-layout -Xfrontend DynamicExtraInhabitantsOneByte -Xfrontend -verify-type-layout -Xfrontend DynamicExtraInhabitantsTwoBytes -O -o %t/a.out %s
// RUN: %target-run %t/a.out 2>&1

// Type layout verifier is only compiled into the runtime in asserts builds.
// REQUIRES: swift_stdlib_asserts

// REQUIRES: executable_test

// CHECK-NOT: Type verification


import Swift
import StdlibUnittest

enum SpareBitExtraInhabitants {
  case a(Builtin.Int30)
  case b(Builtin.Int30)
}

enum SpareBitSingleExtraInhabitant {
  case a(Builtin.Int30)
  case b(Builtin.Int30)
  case c(Builtin.Int30)
}

enum SpareBitNoExtraInhabitant {
  case a(Builtin.Int30)
  case b(Builtin.Int30)
  case c(Builtin.Int30)
  case d(Builtin.Int30)
}

enum SpareBitNoExtraInhabitant2 {
  case a(Builtin.Int30)
  case b(Builtin.Int30)
  case c(Builtin.Int30)
  case d
}

enum TwoTagExtraInhabitants {
  case a(Builtin.Int32)
  case b(Builtin.Int32)
}

enum ThreeTagExtraInhabitants {
  case a(Builtin.Int32)
  case b(Builtin.Int32)
  case c(Builtin.Int32)
}

enum NoTagExtraInhabitants {
  case aaa(Builtin.Int32), aab(Builtin.Int32), aac(Builtin.Int32), aad(Builtin.Int32), aae(Builtin.Int32), aaf(Builtin.Int32), aag(Builtin.Int32), aah(Builtin.Int32)
  case aba(Builtin.Int32), abb(Builtin.Int32), abc(Builtin.Int32), abd(Builtin.Int32), abe(Builtin.Int32), abf(Builtin.Int32), abg(Builtin.Int32), abh(Builtin.Int32)
  case aca(Builtin.Int32), acb(Builtin.Int32), acc(Builtin.Int32), acd(Builtin.Int32), ace(Builtin.Int32), acf(Builtin.Int32), acg(Builtin.Int32), ach(Builtin.Int32)
  case ada(Builtin.Int32), adb(Builtin.Int32), adc(Builtin.Int32), add(Builtin.Int32), ade(Builtin.Int32), adf(Builtin.Int32), adg(Builtin.Int32), adh(Builtin.Int32)
  case aea(Builtin.Int32), aeb(Builtin.Int32), aec(Builtin.Int32), aed(Builtin.Int32), aee(Builtin.Int32), aef(Builtin.Int32), aeg(Builtin.Int32), aeh(Builtin.Int32)
  case afa(Builtin.Int32), afb(Builtin.Int32), afc(Builtin.Int32), afd(Builtin.Int32), afe(Builtin.Int32), aff(Builtin.Int32), afg(Builtin.Int32), afh(Builtin.Int32)
  case aga(Builtin.Int32), agb(Builtin.Int32), agc(Builtin.Int32), agd(Builtin.Int32), age(Builtin.Int32), agf(Builtin.Int32), agg(Builtin.Int32), agh(Builtin.Int32)
  case aha(Builtin.Int32), ahb(Builtin.Int32), ahc(Builtin.Int32), ahd(Builtin.Int32), ahe(Builtin.Int32), ahf(Builtin.Int32), ahg(Builtin.Int32), ahh(Builtin.Int32)

  case baa(Builtin.Int32), bab(Builtin.Int32), bac(Builtin.Int32), bad(Builtin.Int32), bae(Builtin.Int32), baf(Builtin.Int32), bag(Builtin.Int32), bah(Builtin.Int32)
  case bba(Builtin.Int32), bbb(Builtin.Int32), bbc(Builtin.Int32), bbd(Builtin.Int32), bbe(Builtin.Int32), bbf(Builtin.Int32), bbg(Builtin.Int32), bbh(Builtin.Int32)
  case bca(Builtin.Int32), bcb(Builtin.Int32), bcc(Builtin.Int32), bcd(Builtin.Int32), bce(Builtin.Int32), bcf(Builtin.Int32), bcg(Builtin.Int32), bch(Builtin.Int32)
  case bda(Builtin.Int32), bdb(Builtin.Int32), bdc(Builtin.Int32), bdd(Builtin.Int32), bde(Builtin.Int32), bdf(Builtin.Int32), bdg(Builtin.Int32), bdh(Builtin.Int32)
  case bea(Builtin.Int32), beb(Builtin.Int32), bec(Builtin.Int32), bed(Builtin.Int32), bee(Builtin.Int32), bef(Builtin.Int32), beg(Builtin.Int32), beh(Builtin.Int32)
  case bfa(Builtin.Int32), bfb(Builtin.Int32), bfc(Builtin.Int32), bfd(Builtin.Int32), bfe(Builtin.Int32), bff(Builtin.Int32), bfg(Builtin.Int32), bfh(Builtin.Int32)
  case bga(Builtin.Int32), bgb(Builtin.Int32), bgc(Builtin.Int32), bgd(Builtin.Int32), bge(Builtin.Int32), bgf(Builtin.Int32), bgg(Builtin.Int32), bgh(Builtin.Int32)
  case bha(Builtin.Int32), bhb(Builtin.Int32), bhc(Builtin.Int32), bhd(Builtin.Int32), bhe(Builtin.Int32), bhf(Builtin.Int32), bhg(Builtin.Int32), bhh(Builtin.Int32)

  case caa(Builtin.Int32), cab(Builtin.Int32), cac(Builtin.Int32), cad(Builtin.Int32), cae(Builtin.Int32), caf(Builtin.Int32), cag(Builtin.Int32), cah(Builtin.Int32)
  case cba(Builtin.Int32), cbb(Builtin.Int32), cbc(Builtin.Int32), cbd(Builtin.Int32), cbe(Builtin.Int32), cbf(Builtin.Int32), cbg(Builtin.Int32), cbh(Builtin.Int32)
  case cca(Builtin.Int32), ccb(Builtin.Int32), ccc(Builtin.Int32), ccd(Builtin.Int32), cce(Builtin.Int32), ccf(Builtin.Int32), ccg(Builtin.Int32), cch(Builtin.Int32)
  case cda(Builtin.Int32), cdb(Builtin.Int32), cdc(Builtin.Int32), cdd(Builtin.Int32), cde(Builtin.Int32), cdf(Builtin.Int32), cdg(Builtin.Int32), cdh(Builtin.Int32)
  case cea(Builtin.Int32), ceb(Builtin.Int32), cec(Builtin.Int32), ced(Builtin.Int32), cee(Builtin.Int32), cef(Builtin.Int32), ceg(Builtin.Int32), ceh(Builtin.Int32)
  case cfa(Builtin.Int32), cfb(Builtin.Int32), cfc(Builtin.Int32), cfd(Builtin.Int32), cfe(Builtin.Int32), cff(Builtin.Int32), cfg(Builtin.Int32), cfh(Builtin.Int32)
  case cga(Builtin.Int32), cgb(Builtin.Int32), cgc(Builtin.Int32), cgd(Builtin.Int32), cge(Builtin.Int32), cgf(Builtin.Int32), cgg(Builtin.Int32), cgh(Builtin.Int32)
  case cha(Builtin.Int32), chb(Builtin.Int32), chc(Builtin.Int32), chd(Builtin.Int32), che(Builtin.Int32), chf(Builtin.Int32), chg(Builtin.Int32), chh(Builtin.Int32)

  case daa(Builtin.Int32), dab(Builtin.Int32), dac(Builtin.Int32), dad(Builtin.Int32), dae(Builtin.Int32), daf(Builtin.Int32), dag(Builtin.Int32), dah(Builtin.Int32)
  case dba(Builtin.Int32), dbb(Builtin.Int32), dbc(Builtin.Int32), dbd(Builtin.Int32), dbe(Builtin.Int32), dbf(Builtin.Int32), dbg(Builtin.Int32), dbh(Builtin.Int32)
  case dca(Builtin.Int32), dcb(Builtin.Int32), dcc(Builtin.Int32), dcd(Builtin.Int32), dce(Builtin.Int32), dcf(Builtin.Int32), dcg(Builtin.Int32), dch(Builtin.Int32)
  case dda(Builtin.Int32), ddb(Builtin.Int32), ddc(Builtin.Int32), ddd(Builtin.Int32), dde(Builtin.Int32), ddf(Builtin.Int32), ddg(Builtin.Int32), ddh(Builtin.Int32)
  case dea(Builtin.Int32), deb(Builtin.Int32), dec(Builtin.Int32), ded(Builtin.Int32), dee(Builtin.Int32), def(Builtin.Int32), deg(Builtin.Int32), deh(Builtin.Int32)
  case dfa(Builtin.Int32), dfb(Builtin.Int32), dfc(Builtin.Int32), dfd(Builtin.Int32), dfe(Builtin.Int32), dff(Builtin.Int32), dfg(Builtin.Int32), dfh(Builtin.Int32)
  case dga(Builtin.Int32), dgb(Builtin.Int32), dgc(Builtin.Int32), dgd(Builtin.Int32), dge(Builtin.Int32), dgf(Builtin.Int32), dgg(Builtin.Int32), dgh(Builtin.Int32)
  case dha(Builtin.Int32), dhb(Builtin.Int32), dhc(Builtin.Int32), dhd(Builtin.Int32), dhe(Builtin.Int32), dhf(Builtin.Int32), dhg(Builtin.Int32), dhh(Builtin.Int32)
}

enum DynamicExtraInhabitants<T> {
  case payloadA(T)
  case payloadB(T)

  case tagAAA, tagAAB, tagAAC, tagAAD, tagAAE, tagAAF, tagAAG, tagAAH
  case tagABA, tagABB, tagABC, tagABD, tagABE, tagABF, tagABG, tagABH
  case tagACA, tagACB, tagACC, tagACD, tagACE, tagACF, tagACG, tagACH
  case tagADA, tagADB, tagADC, tagADD, tagADE, tagADF, tagADG, tagADH
  case tagAEA, tagAEB, tagAEC, tagAED, tagAEE, tagAEF, tagAEG, tagAEH
  case tagAFA, tagAFB, tagAFC, tagAFD, tagAFE, tagAFF, tagAFG, tagAFH
  case tagAGA, tagAGB, tagAGC, tagAGD, tagAGE, tagAGF, tagAGG, tagAGH
  case tagAHA, tagAHB, tagAHC, tagAHD, tagAHE, tagAHF, tagAHG, tagAHH
  case tagAIA, tagAIB, tagAIC, tagAID, tagAIE, tagAIF, tagAIG, tagAIH

  case tagBAA, tagBAB, tagBAC, tagBAD, tagBAE, tagBAF, tagBAG, tagBAH
  case tagBBA, tagBBB, tagBBC, tagBBD, tagBBE, tagBBF, tagBBG, tagBBH
  case tagBCA, tagBCB, tagBCC, tagBCD, tagBCE, tagBCF, tagBCG, tagBCH
  case tagBDA, tagBDB, tagBDC, tagBDD, tagBDE, tagBDF, tagBDG, tagBDH
  case tagBEA, tagBEB, tagBEC, tagBED, tagBEE, tagBEF, tagBEG, tagBEH
  case tagBFA, tagBFB, tagBFC, tagBFD, tagBFE, tagBFF, tagBFG, tagBFH
  case tagBGA, tagBGB, tagBGC, tagBGD, tagBGE, tagBGF, tagBGG, tagBGH
  case tagBHA, tagBHB, tagBHC, tagBHD, tagBHE, tagBHF, tagBHG, tagBHH
  case tagBIA, tagBIB, tagBIC, tagBID, tagBIE, tagBIF, tagBIG, tagBIH

  case tagCAA, tagCAB, tagCAC, tagCAD, tagCAE, tagCAF, tagCAG, tagCAH
  case tagCBA, tagCBB, tagCBC, tagCBD, tagCBE, tagCBF, tagCBG, tagCBH
  case tagCCA, tagCCB, tagCCC, tagCCD, tagCCE, tagCCF, tagCCG, tagCCH
  case tagCDA, tagCDB, tagCDC, tagCDD, tagCDE, tagCDF, tagCDG, tagCDH
  case tagCEA, tagCEB, tagCEC, tagCED, tagCEE, tagCEF, tagCEG, tagCEH
  case tagCFA, tagCFB, tagCFC, tagCFD, tagCFE, tagCFF, tagCFG, tagCFH
  case tagCGA, tagCGB, tagCGC, tagCGD, tagCGE, tagCGF, tagCGG, tagCGH
  case tagCHA, tagCHB, tagCHC, tagCHD, tagCHE, tagCHF, tagCHG, tagCHH
  case tagCIA, tagCIB, tagCIC, tagCID, tagCIE, tagCIF, tagCIG, tagCIH

  case tagDAA, tagDAB, tagDAC, tagDAD, tagDAE, tagDAF, tagDAG, tagDAH
  case tagDBA, tagDBB, tagDBC, tagDBD, tagDBE, tagDBF, tagDBG, tagDBH
  case tagDCA, tagDCB, tagDCC, tagDCD, tagDCE, tagDCF, tagDCG, tagDCH
  case tagDDA, tagDDB, tagDDC, tagDDD, tagDDE, tagDDF, tagDDG, tagDDH
  case tagDEA, tagDEB, tagDEC, tagDED, tagDEE, tagDEF, tagDEG, tagDEH
  case tagDFA, tagDFB, tagDFC, tagDFD, tagDFE, tagDFF, tagDFG, tagDFH
  case tagDGA, tagDGB, tagDGC, tagDGD, tagDGE, tagDGF, tagDGG, tagDGH
  case tagDHA, tagDHB, tagDHC, tagDHD, tagDHE, tagDHF, tagDHG, tagDHH
  case tagDIA, tagDIB, tagDIC, tagDID, tagDIE, tagDIF, tagDIG, tagDIH
}

typealias DynamicExtraInhabitantsNever = DynamicExtraInhabitants<Never>
typealias DynamicExtraInhabitantsZeroBytes = DynamicExtraInhabitants<()>
typealias DynamicExtraInhabitantsOneByte = DynamicExtraInhabitants<UInt8>
typealias DynamicExtraInhabitantsTwoBytes = DynamicExtraInhabitants<UInt16>

var tests = TestSuite("extra inhabitants of structs")

@inline(never)
func expectHasAtLeastTwoExtraInhabitants<T>(_: T.Type,
                                            nil theNil: T??,
                                            someNil: T??,
                                 file: String = #file, line: UInt = #line) {
  expectEqual(MemoryLayout<T>.size, MemoryLayout<T??>.size,
              "\(T.self) has at least two extra inhabitants",
              file: file, line: line)

  expectNil(theNil,
            "\(T.self) extra inhabitant should agree in generic and concrete " +
            "context")

  expectNil(someNil!,
            "\(T.self) extra inhabitant should agree in generic and concrete " +
            "context")
}

@inline(never)
func expectHasExtraInhabitant<T>(_: T.Type, nil theNil: T?,
                                 file: String = #file, line: UInt = #line) {
  expectEqual(MemoryLayout<T>.size, MemoryLayout<T?>.size,
              "\(T.self) has extra inhabitant",
              file: file, line: line)

  expectNil(theNil,
            "\(T.self) extra inhabitant should agree in generic and concrete " +
            "context")
}

func expectHasNoExtraInhabitant<T>(_: T.Type,
                                   file: String = #file, line: UInt = #line) {
  expectNotEqual(MemoryLayout<T>.size, MemoryLayout<T?>.size,
                 "\(T.self) does not have extra inhabitant",
                 file: file, line: line)
}

tests.test("types that have no extra inhabitant") {
  expectHasNoExtraInhabitant(SpareBitNoExtraInhabitant.self)
  expectHasNoExtraInhabitant(SpareBitNoExtraInhabitant2.self)
  expectHasNoExtraInhabitant(NoTagExtraInhabitants.self)
}
tests.test("types that have at least one extra inhabitant") {
  expectHasExtraInhabitant(SpareBitExtraInhabitants.self, nil: nil)
  expectHasExtraInhabitant(SpareBitSingleExtraInhabitant.self, nil: nil)
  expectHasExtraInhabitant(TwoTagExtraInhabitants.self, nil: nil)
  expectHasExtraInhabitant(ThreeTagExtraInhabitants.self, nil: nil)
  expectHasExtraInhabitant(DynamicExtraInhabitantsNever.self, nil: nil)
  expectHasExtraInhabitant(DynamicExtraInhabitantsZeroBytes.self, nil: nil)
  expectHasExtraInhabitant(DynamicExtraInhabitantsOneByte.self, nil: nil)
  expectHasExtraInhabitant(DynamicExtraInhabitantsTwoBytes.self, nil: nil)
}
tests.test("types that have at least two extra inhabitants") {
  expectHasAtLeastTwoExtraInhabitants(SpareBitExtraInhabitants.self, nil: nil, someNil: .some(nil))
  expectHasAtLeastTwoExtraInhabitants(TwoTagExtraInhabitants.self, nil: nil, someNil: .some(nil))
  expectHasAtLeastTwoExtraInhabitants(ThreeTagExtraInhabitants.self, nil: nil, someNil: .some(nil))
  expectHasAtLeastTwoExtraInhabitants(DynamicExtraInhabitantsNever.self, nil: nil, someNil: .some(nil))
  expectHasAtLeastTwoExtraInhabitants(DynamicExtraInhabitantsZeroBytes.self, nil: nil, someNil: .some(nil))
  expectHasAtLeastTwoExtraInhabitants(DynamicExtraInhabitantsOneByte.self, nil: nil, someNil: .some(nil))
  expectHasAtLeastTwoExtraInhabitants(DynamicExtraInhabitantsTwoBytes.self, nil: nil, someNil: .some(nil))
}
runAllTests()
