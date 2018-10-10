//===--- StringComparison.swift -------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

////////////////////////////////////////////////////////////////////////////////
// WARNING: This file is manually generated from .gyb template and should not
// be directly modified. Instead, make changes to StringComparison.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

//
// Test String iteration performance over a variety of workloads, languages,
// and symbols.
//

import TestsUtils

extension String {
  func lines() -> [String] {
    return self.split(separator: "\n").map { String($0) }
  }
}


public let StringComparison = [
  BenchmarkInfo(
    name: "StringComparison_ascii",
    runFunction: run_StringComparison_ascii,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_ascii) }
  ),
  BenchmarkInfo(
    name: "StringComparison_latin1",
    runFunction: run_StringComparison_latin1,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_latin1) }
  ),
  BenchmarkInfo(
    name: "StringComparison_fastPrenormal",
    runFunction: run_StringComparison_fastPrenormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_fastPrenormal) }
  ),
  BenchmarkInfo(
    name: "StringComparison_slowerPrenormal",
    runFunction: run_StringComparison_slowerPrenormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_slowerPrenormal) }
  ),
  BenchmarkInfo(
    name: "StringComparison_nonBMPSlowestPrenormal",
    runFunction: run_StringComparison_nonBMPSlowestPrenormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_nonBMPSlowestPrenormal) }
  ),
  BenchmarkInfo(
    name: "StringComparison_emoji",
    runFunction: run_StringComparison_emoji,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_emoji) }
  ),
  BenchmarkInfo(
    name: "StringComparison_abnormal",
    runFunction: run_StringComparison_abnormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_abnormal) }
  ),
  BenchmarkInfo(
    name: "StringComparison_zalgo",
    runFunction: run_StringComparison_zalgo,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_zalgo) }
  ),
  BenchmarkInfo(
    name: "StringComparison_longSharedPrefix",
    runFunction: run_StringComparison_longSharedPrefix,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_longSharedPrefix) }
  ),
]

public let StringHashing = [
  BenchmarkInfo(
    name: "StringHashing_ascii",
    runFunction: run_StringHashing_ascii,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_ascii) }
  ),
  BenchmarkInfo(
    name: "StringHashing_latin1",
    runFunction: run_StringHashing_latin1,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_latin1) }
  ),
  BenchmarkInfo(
    name: "StringHashing_fastPrenormal",
    runFunction: run_StringHashing_fastPrenormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_fastPrenormal) }
  ),
  BenchmarkInfo(
    name: "StringHashing_slowerPrenormal",
    runFunction: run_StringHashing_slowerPrenormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_slowerPrenormal) }
  ),
  BenchmarkInfo(
    name: "StringHashing_nonBMPSlowestPrenormal",
    runFunction: run_StringHashing_nonBMPSlowestPrenormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_nonBMPSlowestPrenormal) }
  ),
  BenchmarkInfo(
    name: "StringHashing_emoji",
    runFunction: run_StringHashing_emoji,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_emoji) }
  ),
  BenchmarkInfo(
    name: "StringHashing_abnormal",
    runFunction: run_StringHashing_abnormal,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_abnormal) }
  ),
  BenchmarkInfo(
    name: "StringHashing_zalgo",
    runFunction: run_StringHashing_zalgo,
    tags: [.validation, .api, .String],
    setUpFunction: { blackHole(Workload_zalgo) }
  ),
]

public let NormalizedIterator = [
  BenchmarkInfo(
    name: "NormalizedIterator_ascii",
    runFunction: run_NormalizedIterator_ascii,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_ascii) }
  ),
  BenchmarkInfo(
    name: "NormalizedIterator_latin1",
    runFunction: run_NormalizedIterator_latin1,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_latin1) }
  ),
  BenchmarkInfo(
    name: "NormalizedIterator_fastPrenormal",
    runFunction: run_NormalizedIterator_fastPrenormal,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_fastPrenormal) }
  ),
  BenchmarkInfo(
    name: "NormalizedIterator_slowerPrenormal",
    runFunction: run_NormalizedIterator_slowerPrenormal,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_slowerPrenormal) }
  ),
  BenchmarkInfo(
    name: "NormalizedIterator_nonBMPSlowestPrenormal",
    runFunction: run_NormalizedIterator_nonBMPSlowestPrenormal,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_nonBMPSlowestPrenormal) }
  ),
  BenchmarkInfo(
    name: "NormalizedIterator_emoji",
    runFunction: run_NormalizedIterator_emoji,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_emoji) }
  ),
  BenchmarkInfo(
    name: "NormalizedIterator_abnormal",
    runFunction: run_NormalizedIterator_abnormal,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_abnormal) }
  ),
  BenchmarkInfo(
    name: "NormalizedIterator_zalgo",
    runFunction: run_NormalizedIterator_zalgo,
    tags: [.validation, .String],
    setUpFunction: { blackHole(Workload_zalgo) }
  ),
]

var Workload_ascii: Workload! = Workload.ascii

var Workload_latin1: Workload! = Workload.latin1

var Workload_fastPrenormal: Workload! = Workload.fastPrenormal

var Workload_slowerPrenormal: Workload! = Workload.slowerPrenormal

var Workload_nonBMPSlowestPrenormal: Workload! = Workload.nonBMPSlowestPrenormal

var Workload_emoji: Workload! = Workload.emoji

var Workload_abnormal: Workload! = Workload.abnormal

var Workload_zalgo: Workload! = Workload.zalgo

var Workload_longSharedPrefix: Workload! = Workload.longSharedPrefix


@inline(never)
public func run_StringComparison_ascii(_ N: Int) {
  let workload: Workload = Workload_ascii
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_latin1(_ N: Int) {
  let workload: Workload = Workload_latin1
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_fastPrenormal(_ N: Int) {
  let workload: Workload = Workload_fastPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_slowerPrenormal(_ N: Int) {
  let workload: Workload = Workload_slowerPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_nonBMPSlowestPrenormal(_ N: Int) {
  let workload: Workload = Workload_nonBMPSlowestPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_emoji(_ N: Int) {
  let workload: Workload = Workload_emoji
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_abnormal(_ N: Int) {
  let workload: Workload = Workload_abnormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_zalgo(_ N: Int) {
  let workload: Workload = Workload_zalgo
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
public func run_StringComparison_longSharedPrefix(_ N: Int) {
  let workload: Workload = Workload_longSharedPrefix
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}


@inline(never)
public func run_StringHashing_ascii(_ N: Int) {
  let workload: Workload = Workload.ascii
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}

@inline(never)
public func run_StringHashing_latin1(_ N: Int) {
  let workload: Workload = Workload.latin1
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}

@inline(never)
public func run_StringHashing_fastPrenormal(_ N: Int) {
  let workload: Workload = Workload.fastPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}

@inline(never)
public func run_StringHashing_slowerPrenormal(_ N: Int) {
  let workload: Workload = Workload.slowerPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}

@inline(never)
public func run_StringHashing_nonBMPSlowestPrenormal(_ N: Int) {
  let workload: Workload = Workload.nonBMPSlowestPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}

@inline(never)
public func run_StringHashing_emoji(_ N: Int) {
  let workload: Workload = Workload.emoji
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}

@inline(never)
public func run_StringHashing_abnormal(_ N: Int) {
  let workload: Workload = Workload.abnormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}

@inline(never)
public func run_StringHashing_zalgo(_ N: Int) {
  let workload: Workload = Workload.zalgo
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      blackHole(str.hashValue)
    }
  }
}


@inline(never)
public func run_NormalizedIterator_ascii(_ N: Int) {
  let workload: Workload = Workload.ascii
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}

@inline(never)
public func run_NormalizedIterator_latin1(_ N: Int) {
  let workload: Workload = Workload.latin1
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}

@inline(never)
public func run_NormalizedIterator_fastPrenormal(_ N: Int) {
  let workload: Workload = Workload.fastPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}

@inline(never)
public func run_NormalizedIterator_slowerPrenormal(_ N: Int) {
  let workload: Workload = Workload.slowerPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}

@inline(never)
public func run_NormalizedIterator_nonBMPSlowestPrenormal(_ N: Int) {
  let workload: Workload = Workload.nonBMPSlowestPrenormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}

@inline(never)
public func run_NormalizedIterator_emoji(_ N: Int) {
  let workload: Workload = Workload.emoji
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}

@inline(never)
public func run_NormalizedIterator_abnormal(_ N: Int) {
  let workload: Workload = Workload.abnormal
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}

@inline(never)
public func run_NormalizedIterator_zalgo(_ N: Int) {
  let workload: Workload = Workload.zalgo
  let tripCount = workload.tripCount
  let payload = workload.payload
  for _ in 1...tripCount*N {
    for str in payload {
      str._withNFCCodeUnits { cu in
        blackHole(cu)
      }
    }
  }
}


struct Workload {
  static let N = 100

  let name: String
  let payload: [String]
  var scaleMultiplier: Double

  init(name: String, payload: [String], scaleMultiplier: Double = 1.0) {
    self.name = name
    self.payload = payload
    self.scaleMultiplier = scaleMultiplier
  }

  var tripCount: Int {
    return Int(Double(Workload.N) * scaleMultiplier)
  }

  static let ascii = Workload(
    name: "ASCII",
    payload: """
      woodshed
      lakism
      gastroperiodynia
      afetal
      Casearia
      ramsch
      Nickieben
      undutifulness
      decorticate
      neognathic
      mentionable
      tetraphenol
      pseudonymal
      dislegitimate
      Discoidea
      criminative
      disintegratory
      executer
      Cylindrosporium
      complimentation
      Ixiama
      Araceae
      silaginoid
      derencephalus
      Lamiidae
      marrowlike
      ninepin
      trihemimer
      semibarbarous
      heresy
      existence
      fretless
      Amiranha
      handgravure
      orthotropic
      Susumu
      teleutospore
      sleazy
      shapeliness
      hepatotomy
      exclusivism
      stifler
      cunning
      isocyanuric
      pseudepigraphy
      carpetbagger
      unglory
      """.lines(),
      scaleMultiplier: 0.25
  )

  static let latin1 = Workload(
    name: "Latin1",
    payload: """
      café
      résumé
      caférésumé
      ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º
      1+1=3
      ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹
      ¡¢£¤¥¦§¨©ª«¬­®
      »¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍ
      ÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãä
      åæçèéêëìíîïðñò
      ÎÏÐÑÒÓÔÕÖëìíîïðñò
      óôõö÷øùúûüýþÿ
      123.456£=>¥
      123.456
      """.lines()
  )
  static let fastPrenormal = Workload(
    name: "FastPrenormal",
    payload: """
      ĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥ
      ĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸ
      ĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲ
      ųŴŵŶŷŸŹźŻżŽžſƀƁƂƃƄƅƆ
      ƇƈƉƊƋƌƍƎƏƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿǀ
      Ƈ
      ǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏǐǑǒǓǔǕǖ
      ǗǘǙǚǛǜǝǞǟǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑ
      ȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪȫȬ
      ȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪȫȬǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑ
      ȭȮȯȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯɰ
      ɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃʄ
      ɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃ
      ʅʆʇʈʉʊʋʌʍʎʏʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯʰ
      ʱʲʳʴʵʶʷʸʹʺʻʼʽʾʿˀˁ˂˃˄˅ˆˇˈˉˊˋˌˍˎˏːˑ˒˓˔˕˖˗˘˙˚˛˜˝˞˟ˠˡˢˣˤ˥˦
      ˧˨˩˪˫ˬ˭ˮ˯˰˱˲˳˴˵˶˷˸˹˺˻˼˽˾
      """.lines()
  )
  static let slowerPrenormal = Workload(
    name: "SlowerPrenormal",
    payload: """
      Swiftに大幅な改良が施され、
      安定していてしかも
      直感的に使うことができる
      向けプログラミング言語になりました。
      이번 업데이트에서는 강력하면서도
      \u{201c}Hello\u{2010}world\u{2026}\u{201d}
      平台的编程语言
      功能强大且直观易用
      而本次更新对其进行了全面优化
      в чащах юга жил-был цитрус
      \u{300c}\u{300e}今日は\u{3001}世界\u{3002}\u{300f}\u{300d}
      но фальшивый экземпляр
      """.lines()
  )
  // static let slowestPrenormal = """
  //   """.lines()
  static let nonBMPSlowestPrenormal = Workload(
    name: "NonBMPSlowestPrenormal",
    payload: """
      𓀀𓀤𓁓𓁲𓃔𓃗
      𓀀𓀁𓀂𓀃𓀄𓀇𓀈𓀉𓀊𓀋𓀌𓀍𓀎𓀏𓀓𓀔𓀕𓀖𓀗𓀘𓀙𓀚𓀛𓀜𓀞𓀟𓀠𓀡𓀢𓀣
      𓀤𓀥𓀦𓀧𓀨𓀩𓀪𓀫𓀬𓀭
      𓁡𓁢𓁣𓁤𓁥𓁦𓁧𓁨𓁩𓁫𓁬𓁭𓁮𓁯𓁰𓁱𓁲𓁳𓁴𓁵𓁶𓁷𓁸
      𓁹𓁺𓁓𓁔𓁕𓁻𓁼𓁽𓁾𓁿
      𓀀𓀁𓀂𓀃𓀄𓃒𓃓𓃔𓃕𓃻𓃼𓃽𓃾𓃿𓄀𓄁𓄂𓄃𓄄𓄅𓄆𓄇𓄈𓄉𓄊𓄋𓄌𓄍𓄎
      𓂿𓃀𓃁𓃂𓃃𓃄𓃅
      𓃘𓃙𓃚𓃛𓃠𓃡𓃢𓃣𓃦𓃧𓃨𓃩𓃬𓃭𓃮𓃯𓃰𓃲𓃳𓃴𓃵𓃶𓃷𓃸
      𓃘𓃙𓃚𓃛𓃠𓃡𓃢𓃣𓃦𓃧𓃨𓃩𓃬𓃭𓃮𓃯𓃰𓃲𓃳𓃴𓃵𓃶𓃷
      𓀀𓀁𓀂𓀃𓀄𓆇𓆈𓆉𓆊𓆋𓆌𓆍𓆎𓆏𓆐𓆑𓆒𓆓𓆔𓆗𓆘𓆙𓆚𓆛𓆝𓆞𓆟𓆠𓆡𓆢𓆣𓆤
      𓆥𓆦𓆧𓆨𓆩𓆪𓆫𓆬𓆭𓆮𓆯𓆰𓆱𓆲𓆳𓆴𓆵𓆶𓆷𓆸𓆹𓆺𓆻
      """.lines()
  )
  static let emoji = Workload(
    name: "Emoji",
    payload: """
      👍👩‍👩‍👧‍👧👨‍👨‍👦‍👦🇺🇸🇨🇦🇲🇽👍🏻👍🏼👍🏽👍🏾👍🏿
      👍👩‍👩‍👧‍👧👨‍👨‍👦‍👦🇺🇸🇨🇦🇲🇽👍🏿👍🏻👍🏼👍🏽👍🏾
      😀🧀😀😃😄😁🤣😂😅😆
      😺🎃🤖👾😸😹😻😼😾😿🙀😽🙌🙏🤝👍✌🏽
      ☺️😊😇🙂😍😌😉🙃😘😗😙😚😛😝😜
      😋🤑🤗🤓😎😒😏🤠🤡😞😔😟😕😖😣☹️🙁😫😩😤😠😑😐😶😡😯
      😦😧😮😱😳😵😲😨😰😢😥
      😪😓😭🤤😴🙄🤔🤥🤧🤢🤐😬😷🤒🤕😈💩👺👹👿👻💀☠️👽
      """.lines()
  )

  static let abnormal = Workload(
    name: "Abnormal",
    payload: """
    ae\u{301}ae\u{301}ae\u{302}ae\u{303}ae\u{304}ae\u{305}ae\u{306}ae\u{307}
    ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{300}
    \u{f900}\u{f901}\u{f902}\u{f903}\u{f904}\u{f905}\u{f906}\u{f907}\u{f908}\u{f909}\u{f90a}
    \u{f90b}\u{f90c}\u{f90d}\u{f90e}\u{f90f}\u{f910}\u{f911}\u{f912}\u{f913}\u{f914}\u{f915}\u{f916}\u{f917}\u{f918}\u{f919}
    \u{f900}\u{f91a}\u{f91b}\u{f91c}\u{f91d}\u{f91e}\u{f91f}\u{f920}\u{f921}\u{f922}
    """.lines()
  )
  // static let pathological = """
  //   """.lines()
  static let zalgo = Workload(
    name: "Zalgo",
    payload: """
    ṭ̴̵̶̷̸̢̧̨̡̛̤̥̦̩̪̫̬̭̮̯̰̖̗̘̙̜̝̞̟̠̱̲̳̹̺̻̼͇͈͉͍͎̀́̂̃̄̅̆̇̈̉ͣͤ̊̋̌̍̎̏̐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌̕̚͜͟͢͝͞͠͡ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    h̀́̂̃
    è͇͈͉͍͎́̂̃̄̅̆̇̈̉͊͋͌͏̡̢̧̨̛͓͔͕͖͙͚̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭͇͈͉͍͎͐͑͒͗͛̊̋̌̍̎̏̐̑̒̓̔̀́͂̓̈́͆͊͋͌͘̕̚͜͟͝͞͠ͅ͏͓͔͕͖͐͑͒
    q̴̵̶̷̸̡̢̧̨̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇̀́̂̃̄̅̆̇̈̉̊̋̌̍̎̏̐̑̒̓̔̽̾̿̀́͂̓̈́͆̕̚ͅ
    ư̴̵̶̷̸̗̘̙̜̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    ì̡̢̧̨̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̹̺̻̼͇͈͉͍͎́̂̃̄̉̊̋̌̍̎̏̐̑̒̓̽̾̿̀́͂̓̈́͆͊͋͌ͅ͏͓͔͕͖͙͐͑͒͗ͬͭͮ͘
    c̴̵̶̷̸̡̢̧̨̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼̀́̂̃̄̔̽̾̿̀́͂̓̈́͆ͣͤͥͦͧͨͩͪͫͬͭͮ̕̚͢͡ͅ
    k̴̵̶̷̸̡̢̧̨̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇͈͉͍͎̀́̂̃̄̅̆̇̈̉̊̋̌̍̎̏̐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌̕̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    b̴̵̶̷̸̡̢̛̗̘̙̜̝̞̟̠̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    ŗ̴̵̶̷̸̨̛̩̪̫̯̰̱̲̳̹̺̻̼̬̭̮͇̗̘̙̜̝̞̟̤̥̦͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏̡̢͓͔͕͖͙͚̠̣͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    o
    w̗̘͇͈͉͍͎̓̈́͆͊͋͌ͅ͏̛͓͔͕͖͙͚̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡
    n͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ
    f̛̗̘̙̜̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦ͘͜͟͢͝͞͠͡
    ơ̗̘̙̜̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͥͦͧͨͩͪͫͬͭͮ͘
    xͣͤͥͦͧͨͩͪͫͬͭͮ
    """.lines(),
    scaleMultiplier: 0.25
  )
  
  static let longSharedPrefix = Workload(
    name: "LongSharedPrefix",
    payload: """
    http://www.dogbook.com/dog/239495828/friends/mutual/2939493815
    http://www.dogbook.com/dog/239495828/friends/mutual/3910583739
    http://www.dogbook.com/dog/239495828/friends/mutual/3910583739/shared
    http://www.dogbook.com/dog/239495828/friends/mutual/3910583739/shared
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.🤔
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
    🤔Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.🤔
    """.lines()
  )
  
}

// Local Variables:
// eval: (read-only-mode 1)
// End:
