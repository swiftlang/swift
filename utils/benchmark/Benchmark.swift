//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


////////////////////////////////////////////////////////////////////////////////
//
// This file contains a small number of swift benchmarks.
// At the moment we can't serialize large chunks of the swift standard library,
// which prevents us from optimizing user code. In order to evaluate the
// performance of swift we decided to place some benchmarks in the standard
// library and evaluate the optimized versions.
//
// This file will be removed as soon as rdar://14747929 is fixed.
//
////////////////////////////////////////////////////////////////////////////////
@asmname("mach_absolute_time") func __mach_absolute_time__() -> UInt64

var stringBenchmarkWords: String[] = [
  "woodshed",
  "lakism",
  "gastroperiodynia",
  "afetal",
  "ramsch",
  "Nickieben",
  "undutifulness",
  "birdglue",
  "ungentlemanize",
  "menacingly",
  "heterophile",
  "leoparde",
  "Casearia",
  "decorticate",
  "neognathic",
  "mentionable",
  "tetraphenol",
  "pseudonymal",
  "dislegitimate",
  "Discoidea",
  "intitule",
  "ionium",
  "Lotuko",
  "timbering",
  "nonliquidating",
  "oarialgia",
  "Saccobranchus",
  "reconnoiter",
  "criminative",
  "disintegratory",
  "executer",
  "Cylindrosporium",
  "complimentation",
  "Ixiama",
  "Araceae",
  "silaginoid",
  "derencephalus",
  "Lamiidae",
  "marrowlike",
  "ninepin",
  "dynastid",
  "lampfly",
  "feint",
  "trihemimer",
  "semibarbarous",
  "heresy",
  "tritanope",
  "indifferentist",
  "confound",
  "hyperbolaeon",
  "planirostral",
  "philosophunculist",
  "existence",
  "fretless",
  "Leptandra",
  "Amiranha",
  "handgravure",
  "gnash",
  "unbelievability",
  "orthotropic",
  "Susumu",
  "teleutospore",
  "sleazy",
  "shapeliness",
  "hepatotomy",
  "exclusivism",
  "stifler",
  "cunning",
  "isocyanuric",
  "pseudepigraphy",
  "carpetbagger",
  "respectiveness",
  "Jussi",
  "vasotomy",
  "proctotomy",
  "ovatotriangular",
  "aesthetic",
  "schizogamy",
  "disengagement",
  "foray",
  "haplocaulescent",
  "noncoherent",
  "astrocyte",
  "unreverberated",
  "presenile",
  "lanson",
  "enkraal",
  "contemplative",
  "Syun",
  "sartage",
  "unforgot",
  "wyde",
  "homeotransplant",
  "implicational",
  "forerunnership",
  "calcaneum",
  "stomatodeum",
  "pharmacopedia",
  "preconcessive",
  "trypanosomatic",
  "intracollegiate",
  "rampacious",
  "secundipara",
  "isomeric",
  "treehair",
  "pulmonal",
  "uvate",
  "dugway",
  "glucofrangulin",
  "unglory",
  "Amandus",
  "icterogenetic",
  "quadrireme",
  "Lagostomus",
  "brakeroot",
  "anthracemia",
  "fluted",
  "protoelastose",
  "thro",
  "pined",
  "Saxicolinae",
  "holidaymaking",
  "strigil",
  "uncurbed",
  "starling",
  "redeemeress",
  "Liliaceae",
  "imparsonee",
  "obtusish",
  "brushed",
  "mesally",
  "probosciformed",
  "Bourbonesque",
  "histological",
  "caroba",
  "digestion",
  "Vindemiatrix",
  "triactinal",
  "tattling",
  "arthrobacterium",
  "unended",
  "suspectfulness",
  "movelessness",
  "chartist",
  "Corynebacterium",
  "tercer",
  "oversaturation",
  "Congoleum",
  "antiskeptical",
  "sacral",
  "equiradiate",
  "whiskerage",
  "panidiomorphic",
  "unplanned",
  "anilopyrine",
  "Queres",
  "tartronyl",
  "Ing",
  "notehead",
  "finestiller",
  "weekender",
  "kittenhood",
  "competitrix",
  "premillenarian",
  "convergescence",
  "microcoleoptera",
  "slirt",
  "asteatosis",
  "Gruidae",
  "metastome",
  "ambuscader",
  "untugged",
  "uneducated",
  "redistill",
  "rushlight",
  "freakish",
  "dosology",
  "papyrine",
  "iconologist",
  "Bidpai",
  "prophethood",
  "pneumotropic",
  "chloroformize",
  "intemperance",
  "spongiform",
  "superindignant",
  "divider",
  "starlit",
  "merchantish",
  "indexless",
  "unidentifiably",
  "coumarone",
  "nomism",
  "diaphanous",
  "salve",
  "option",
  "anallantoic",
  "paint",
  "thiofurfuran",
  "baddeleyite",
  "Donne",
  "heterogenicity",
  "decess",
  "eschynite",
  "mamma",
  "unmonarchical",
  "Archiplata",
  "widdifow",
  "apathic",
  "overline",
  "chaetophoraceous",
  "creaky",
  "trichosporange",
  "uninterlined",
  "cometwise",
  "hermeneut",
  "unbedraggled",
  "tagged",
  "Sminthurus",
  "somniloquacious",
  "aphasiac",
  "Inoperculata",
  "photoactivity",
  "mobship",
  "unblightedly",
  "lievrite",
  "Khoja",
  "Falerian",
  "milfoil",
  "protectingly",
  "householder",
  "cathedra",
  "calmingly",
  "tordrillite",
  "rearhorse",
  "Leonard",
  "maracock",
  "youngish",
  "kammererite",
  "metanephric",
  "Sageretia",
  "diplococcoid",
  "accelerative",
  "choreal",
  "metalogical",
  "recombination",
  "unimprison",
  "invocation",
  "syndetic",
  "toadback",
  "vaned",
  "cupholder",
  "metropolitanship",
  "paramandelic",
  "dermolysis",
  "Sheriyat",
  "rhabdus",
  "seducee",
  "encrinoid",
  "unsuppliable",
  "cololite",
  "timesaver",
  "preambulate",
  "sampling",
  "roaster",
  "springald",
  "densher",
  "protraditional",
  "naturalesque",
  "Hydrodamalis",
  "cytogenic",
  "shortly",
  "cryptogrammatical",
  "squat",
  "genual",
  "backspier",
  "solubleness",
  "macroanalytical",
  "overcovetousness",
  "Natalie",
  "cuprobismutite",
  "phratriac",
  "Montanize",
  "hymnologist",
  "karyomiton",
  "podger",
  "unofficiousness",
  "antisplasher",
  "supraclavicular",
  "calidity",
  "disembellish",
  "antepredicament",
  "recurvirostral",
  "pulmonifer",
  "coccidial",
  "botonee",
  "protoglobulose",
  "isonym",
  "myeloid",
  "premiership",
  "unmonopolize",
  "unsesquipedalian",
  "unfelicitously",
  "theftbote",
  "undauntable",
  "lob",
  "praenomen",
  "underriver",
  "gorfly",
  "pluckage",
  "radiovision",
  "tyrantship",
  "fraught",
  "doppelkummel",
  "rowan",
  "allosyndetic",
  "kinesiology",
  "psychopath",
  "arrent",
  "amusively",
  "preincorporation",
  "Montargis",
  "pentacron",
  "neomedievalism",
  "sima",
  "lichenicolous",
  "Ecclesiastes",
  "woofed",
  "cardinalist",
  "sandaracin",
  "gymnasial",
  "lithoglyptics",
  "centimeter",
  "quadrupedous",
  "phraseology",
  "tumuli",
  "ankylotomy",
  "myrtol",
  "cohibitive",
  "lepospondylous",
  "silvendy",
  "inequipotential",
  "entangle",
  "raveling",
  "Zeugobranchiata",
  "devastating",
  "grainage",
  "amphisbaenian",
  "blady",
  "cirrose",
  "proclericalism",
  "governmentalist",
  "carcinomorphic",
  "nurtureship",
  "clancular",
  "unsteamed",
  "discernibly",
  "pleurogenic",
  "impalpability",
  "Azotobacterieae",
  "sarcoplasmic",
  "alternant",
  "fitly",
  "acrorrheuma",
  "shrapnel",
  "pastorize",
  "gulflike",
  "foreglow",
  "unrelated",
  "cirriped",
  "cerviconasal",
  "sexuale",
  "pussyfooter",
  "gadolinic",
  "duplicature",
  "codelinquency",
  "trypanolysis",
  "pathophobia",
  "incapsulation",
  "nonaerating",
  "feldspar",
  "diaphonic",
  "epiglottic",
  "depopulator",
  "wisecracker",
  "gravitational",
  "kuba",
  "lactesce",
  "Toxotes",
  "periomphalic",
  "singstress",
  "fannier",
  "counterformula",
  "Acemetae",
  "repugnatorial",
  "collimator",
  "Acinetina",
  "unpeace",
  "drum",
  "tetramorphic",
  "descendentalism",
  "cementer",
  "supraloral",
  "intercostal",
  "Nipponize",
  "negotiator",
  "vacationless",
  "synthol",
  "fissureless",
  "resoap",
  "pachycarpous",
  "reinspiration",
  "misappropriation",
  "disdiazo",
  "unheatable",
  "streng",
  "Detroiter",
  "infandous",
  "loganiaceous",
  "desugar",
  "Matronalia",
  "myxocystoma",
  "Gandhiism",
  "kiddier",
  "relodge",
  "counterreprisal",
  "recentralize",
  "foliously",
  "reprinter",
  "gender",
  "edaciousness",
  "chondriomite",
  "concordant",
  "stockrider",
  "pedary",
  "shikra",
  "blameworthiness",
  "vaccina",
  "Thamnophilinae",
  "wrongwise",
  "unsuperannuated",
  "convalescency",
  "intransmutable",
  "dropcloth",
  "Ceriomyces",
  "ponderal",
  "unstentorian",
  "mem",
  "deceleration",
  "ethionic",
  "untopped",
  "wetback",
  "bebar",
  "undecaying",
  "shoreside",
  "energize",
  "presacral",
  "undismay",
  "agricolite",
  "cowheart",
  "hemibathybian",
  "postexilian",
  "Phacidiaceae",
  "offing",
  "redesignation",
  "skeptically",
  "physicianless",
  "bronchopathy",
  "marabuto",
  "proprietory",
  "unobtruded",
  "funmaker",
  "plateresque",
  "preadventure",
  "beseeching",
  "cowpath",
  "pachycephalia",
  "arthresthesia",
  "supari",
  "lengthily",
  "Nepa",
  "liberation",
  "nigrify",
  "belfry",
  "entoolitic",
  "bazoo",
  "pentachromic",
  "distinguishable",
  "slideable",
  "galvanoscope",
  "remanage",
  "cetene",
  "bocardo",
  "consummation",
  "boycottism",
  "perplexity",
  "astay",
  "Gaetuli",
  "periplastic",
  "consolidator",
  "sluggarding",
  "coracoscapular",
  "anangioid",
  "oxygenizer",
  "Hunanese",
  "seminary",
  "periplast",
  "Corylus",
  "unoriginativeness",
  "persecutee",
  "tweaker",
  "silliness",
  "Dabitis",
  "facetiousness",
  "thymy",
  "nonimperial",
  "mesoblastema",
  "turbiniform",
  "churchway",
  "cooing",
  "frithbot",
  "concomitantly",
  "stalwartize",
  "clingfish",
  "hardmouthed",
  "parallelepipedonal",
  "coracoacromial",
  "factuality",
  "curtilage",
  "arachnoidean",
  "semiaridity",
  "phytobacteriology",
  "premastery",
  "hyperpurist",
  "mobed",
  "opportunistic",
  "acclimature",
  "outdistance",
  "sophister",
  "condonement",
  "oxygenerator",
  "acetonic",
  "emanatory",
  "periphlebitis",
  "nonsociety",
  "spectroradiometric",
  "superaverage",
  "cleanness",
  "posteroventral",
  "unadvised",
  "unmistakedly",
  "pimgenet",
  "auresca",
  "overimitate",
  "dipnoan",
  "chromoxylograph",
  "triakistetrahedron",
  "Suessiones",
  "uncopiable",
  "oligomenorrhea",
  "fribbling",
  "worriable",
  "flot",
  "ornithotrophy",
  "phytoteratology",
  "setup",
  "lanneret",
  "unbraceleted",
  "gudemother",
  "Spica",
  "unconsolatory",
  "recorruption",
  "premenstrual",
  "subretinal",
  "millennialist",
  "subjectibility",
  "rewardproof",
  "counterflight",
  "pilomotor",
  "carpetbaggery",
  "macrodiagonal",
  "slim",
  "indiscernible",
  "cuckoo",
  "moted",
  "controllingly",
  "gynecopathy",
  "porrectus",
  "wanworth",
  "lutfisk",
  "semiprivate",
  "philadelphy",
  "abdominothoracic",
  "coxcomb",
  "dambrod",
  "Metanemertini",
  "balminess",
  "homotypy",
  "waremaker",
  "absurdity",
  "gimcrack",
  "asquat",
  "suitable",
  "perimorphous",
  "kitchenwards",
  "pielum",
  "salloo",
  "paleontologic",
  "Olson",
  "Tellinidae",
  "ferryman",
  "peptonoid",
  "Bopyridae",
  "fallacy",
  "ictuate",
  "aguinaldo",
  "rhyodacite",
  "Ligydidae",
  "galvanometric",
  "acquisitor",
  "muscology",
  "hemikaryon",
  "ethnobotanic",
  "postganglionic",
  "rudimentarily",
  "replenish",
  "phyllorhine",
  "popgunnery",
  "summar",
  "quodlibetary",
  "xanthochromia",
  "autosymbolically",
  "preloreal",
  "extent",
  "strawberry",
  "immortalness",
  "colicwort",
  "frisca",
  "electiveness",
  "heartbroken",
  "affrightingly",
  "reconfiscation",
  "jacchus",
  "imponderably",
  "semantics",
  "beennut",
  "paleometeorological",
  "becost",
  "timberwright",
  "resuppose",
  "syncategorematical",
  "cytolymph",
  "steinbok",
  "explantation",
  "hyperelliptic",
  "antescript",
  "blowdown",
  "antinomical",
  "caravanserai",
  "unweariedly",
  "isonymic",
  "keratoplasty",
  "vipery",
  "parepigastric",
  "endolymphatic",
  "Londonese",
  "necrotomy",
  "angelship",
  "Schizogregarinida",
  "steeplebush",
  "sparaxis",
  "connectedness",
  "tolerance",
  "impingent",
  "agglutinin",
  "reviver",
  "hieroglyphical",
  "dialogize",
  "coestate",
  "declamatory",
  "ventilation",
  "tauromachy",
  "cotransubstantiate",
  "pome",
  "underseas",
  "triquadrantal",
  "preconfinemnt",
  "electroindustrial",
  "selachostomous",
  "nongolfer",
  "mesalike",
  "hamartiology",
  "ganglioblast",
  "unsuccessive",
  "yallow",
  "bacchanalianly",
  "platydactyl",
  "Bucephala",
  "ultraurgent",
  "penalist",
  "catamenial",
  "lynnhaven",
  "unrelevant",
  "lunkhead",
  "metropolitan",
  "hydro",
  "outsoar",
  "vernant",
  "interlanguage",
  "catarrhal",
  "Ionicize",
  "keelless",
  "myomantic",
  "booker",
  "Xanthomonas",
  "unimpeded",
  "overfeminize",
  "speronaro",
  "diaconia",
  "overholiness",
  "liquefacient",
  "Spartium",
  "haggly",
  "albumose",
  "nonnecessary",
  "sulcalization",
  "decapitate",
  "cellated",
  "unguirostral",
  "trichiurid",
  "loveproof",
  "amakebe",
  "screet",
  "arsenoferratin",
  "unfrizz",
  "undiscoverable",
  "procollectivistic",
  "tractile",
  "Winona",
  "dermostosis",
  "eliminant",
  "scomberoid",
  "tensile",
  "typesetting",
  "xylic",
  "dermatopathology",
  "cycloplegic",
  "revocable",
  "fissate",
  "afterplay",
  "screwship",
  "microerg",
  "bentonite",
  "stagecoaching",
  "beglerbeglic",
  "overcharitably",
  "Plotinism",
  "Veddoid",
  "disequalize",
  "cytoproct",
  "trophophore",
  "antidote",
  "allerion",
  "famous",
  "convey",
  "postotic",
  "rapillo",
  "cilectomy",
  "penkeeper",
  "patronym",
  "bravely",
  "ureteropyelitis",
  "Hildebrandine",
  "missileproof",
  "Conularia",
  "deadening",
  "Conrad",
  "pseudochylous",
  "typologically",
  "strummer",
  "luxuriousness",
  "resublimation",
  "glossiness",
  "hydrocauline",
  "anaglyph",
  "personifiable",
  "seniority",
  "formulator",
  "datiscaceous",
  "hydracrylate",
  "Tyranni",
  "Crawthumper",
  "overprove",
  "masher",
  "dissonance",
  "Serpentinian",
  "malachite",
  "interestless",
  "stchi",
  "ogum",
  "polyspermic",
  "archegoniate",
  "precogitation",
  "Alkaphrah",
  "craggily",
  "delightfulness",
  "bioplast",
  "diplocaulescent",
  "neverland",
  "interspheral",
  "chlorhydric",
  "forsakenly",
  "scandium",
  "detubation",
  "telega",
  "Valeriana",
  "centraxonial",
  "anabolite",
  "neger",
  "miscellanea",
  "whalebacker",
  "stylidiaceous",
  "unpropelled",
  "Kennedya",
  "Jacksonite",
  "ghoulish",
  "Dendrocalamus",
  "paynimhood",
  "rappist",
  "unluffed",
  "falling",
  "Lyctus",
  "uncrown",
  "warmly",
  "pneumatism",
  "Morisonian",
  "notate",
  "isoagglutinin",
  "Pelidnota",
  "previsit",
  "contradistinctly",
  "utter",
  "porometer",
  "gie",
  "germanization",
  "betwixt",
  "prenephritic",
  "underpier",
  "Eleutheria",
  "ruthenious",
  "convertor",
  "antisepsin",
  "winterage",
  "tetramethylammonium",
  "Rockaway",
  "Penaea",
  "prelatehood",
  "brisket",
  "unwishful",
  "Minahassa",
  "Briareus",
  "semiaxis",
  "disintegrant",
  "peastick",
  "iatromechanical",
  "fastus",
  "thymectomy",
  "ladyless",
  "unpreened",
  "overflutter",
  "sicker",
  "apsidally",
  "thiazine",
  "guideway",
  "pausation",
  "tellinoid",
  "abrogative",
  "foraminulate",
  "omphalos",
  "Monorhina",
  "polymyarian",
  "unhelpful",
  "newslessness",
  "oryctognosy",
  "octoradial",
  "doxology",
  "arrhythmy",
  "gugal",
  "mesityl",
  "hexaplaric",
  "Cabirian",
  "hordeiform",
  "eddyroot",
  "internarial",
  "deservingness",
  "jawbation",
  "orographically",
  "semiprecious",
  "seasick",
  "thermically",
  "grew",
  "tamability",
  "egotistically",
  "fip",
  "preabsorbent",
  "leptochroa",
  "ethnobotany",
  "podolite",
  "egoistic",
  "semitropical",
  "cero",
  "spinelessness",
  "onshore",
  "omlah",
  "tintinnabulist",
  "machila",
  "entomotomy",
  "nubile",
  "nonscholastic",
  "burnt",
  "Alea",
  "befume",
  "doctorless",
  "Napoleonic",
  "scenting",
  "apokreos",
  "cresylene",
  "paramide",
  "rattery",
  "disinterested",
  "idiopathetic",
  "negatory",
  "fervid",
  "quintato",
  "untricked",
  "Metrosideros",
  "mescaline",
  "midverse",
  "Musophagidae",
  "fictionary",
  "branchiostegous",
  "yoker",
  "residuum",
  "culmigenous",
  "fleam",
  "suffragism",
  "Anacreon",
  "sarcodous",
  "parodistic",
  "writmaking",
  "conversationism",
  "retroposed",
  "tornillo",
  "presuspect",
  "didymous",
  "Saumur",
  "spicing",
  "drawbridge",
  "cantor",
  "incumbrancer",
  "heterospory",
  "Turkeydom",
  "anteprandial",
  "neighbourship",
  "thatchless",
  "drepanoid",
  "lusher",
  "paling",
  "ecthlipsis",
  "heredosyphilitic",
  "although",
  "garetta",
  "temporarily",
  "Monotropa",
  "proglottic",
  "calyptro",
  "persiflage",
  "degradable",
  "paraspecific",
  "undecorative",
  "Pholas",
  "myelon",
  "resteal",
  "quadrantly",
  "scrimped",
  "airer",
  "deviless",
  "caliciform",
  "Sefekhet",
  "shastaite",
  "togate",
  "macrostructure",
  "bipyramid",
  "wey",
  "didynamy",
  "knacker",
  "swage",
  "supermanism",
  "epitheton",
  "overpresumptuous"
  ]

struct RC4 {
  var State : UInt8[]
  var I : UInt8 = 0
  var J : UInt8 = 0

  init() {
    State = new UInt8[256]
  }

  mutating
  func initialize(inout Key: UInt8[]) {
    for i in 0..256 {
      State[i] = UInt8(i)
    }

    var j : UInt8 = 0
    for i in 0..256 {
      var K : UInt8 = Key[i % Key.count]
      var S : UInt8 = State[i]
      j = j &+ S &+ K
      swapByIndex(i, y: Int(j))
    }
  }

  mutating
  func swapByIndex(x: Int, y: Int) {
    let T1 : UInt8 = State[x]
    let T2 : UInt8 = State[y]
    State[x] = T2
    State[y] = T1
  }

  mutating
  func next() -> UInt8 {
    I = I &+ 1
    J = J &+ State[Int(I)]
    swapByIndex(Int(I), y:Int(J))
    return State[Int(State[Int(I)] &+ State[Int(J)]) & 0xFF]
  }

  mutating
  func encrypt(inout Data: UInt8[]) {
    var cnt = Data.count
    for i in 0..cnt {
      Data[i] = Data[i] ^ next()
    }
  }
}

func benchStringSort_internal(inout words: String[]) {
  sort(&words)
}

func benchStringSort() {
  let start = __mach_absolute_time__()
  for i in 0..50 {  // do not change '50', we have historic perf data
    // Notice that we _copy_ the array of words before we sort it.
    benchStringSort_internal(&stringBenchmarkWords)
  }
  let delta = __mach_absolute_time__() - start

  print("\(delta) nanoseconds.")
  print("\(Double(delta) / Double(50)) nanoseconds/lap")
}

func benchRC4_internal(messageLen : Int, iterations : Int, validate : Bool) {
  var Secret = "This is my secret message"
  var Key    = "This is my key"
  var SecretData : UInt8[] = Array(Secret.utf8)
  var KeyData    : UInt8[] = Array(Key.utf8)

  var LongData : UInt8[] = new UInt8[messageLen]

  // Generate a long message. 
  for i in 0..messageLen {
    LongData[i] = SecretData[i % SecretData.count]
  }

  // Generate the Encryptor and Decryptor classes.
  // FIXME: Passing KeyData to the c'tor does not type check.
  var Enc = RC4()
  var Dec = RC4()
  Enc.initialize(&KeyData)
  Dec.initialize(&KeyData)

  let start = __mach_absolute_time__()

  for i in 0..iterations {
    Enc.encrypt(&LongData)
    Dec.encrypt(&LongData)
  }

  let delta = __mach_absolute_time__() - start
  print("\(delta) nanoseconds.")
  print("\(Double(delta) / Double(iterations)) nanoseconds/lap")


  if (validate) {
    print("Validating ...")
    for i in 0..messageLen {
      if (LongData[i] != SecretData[i % SecretData.count]) {
        print("Error at \(i)");
      }
    }
  }
}


func benchRC4() {
  benchRC4_internal(5000, 2000, false)  // do not change 2nd param, we have historic perf data
}

func benchStringComplexWalk() -> Int {
  var s = "निरन्तरान्धकारिता-दिगन्तर-कन्दलदमन्द-सुधारस-बिन्दु-सान्द्रतर-घनाघन-वृन्द-सन्देहकर-स्यन्दमान-मकरन्द-बिन्दु-बन्धुरतर-माकन्द-तरु-कुल-तल्प-कल्प-मृदुल-सिकता-जाल-जटिल-मूल-तल-मरुवक-मिलदलघु-लघु-लय-कलित-रमणीय-पानीय-शालिका-बालिका-करार-विन्द-गलन्तिका-गलदेला-लवङ्ग-पाटल-घनसार-कस्तूरिकातिसौरभ-मेदुर-लघुतर-मधुर-शीतलतर-सलिलधारा-निराकरिष्णु-तदीय-विमल-विलोचन-मयूख-रेखापसारित-पिपासायास-पथिक-लोकान्"


  let start = __mach_absolute_time__()
  var count = 0
  let laps = 10000 // do not change this, we have historic perf data
  for i in 0..laps {
    for c in s.unicodeScalars {
      count++
    }
  }
  let delta = __mach_absolute_time__() - start
  print("\(delta) nanoseconds.")
  print("\(Double(delta) / Double(laps)) nanoseconds/lap")
  return count
}

func benchStringWalk() -> Int {
  let s = "siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig"

  let start = __mach_absolute_time__()
  var count = 0
  let laps = 100000 // do not change this, we have historic perf data
  for i in 0..laps {
    for c in s.unicodeScalars {
      count++
    }
  }
  let delta = __mach_absolute_time__() - start
  print("\(delta) nanoseconds.")
  print("\(Double(delta) / Double(laps)) nanoseconds/lap")
  return count
}

// Source code for this benchmark was ported from the "Great Computer Language
// Shootout". http://dada.perl.it/shootout/objinst_allsrc.html
class Toggle {
    var state : Bool = true
    init(start_state : Bool) {
      state = start_state
    }

    func value() -> Bool {
      return state
    }

    func activate() -> Toggle {
      state = !state
      return self
    }
}

class NthToggle : Toggle {
    var count_max : Int = 0
    var counter  : Int = 0

    init(start_state : Bool, max_counter : Int) {
        super.init(start_state: start_state)
        count_max = max_counter
        counter = 0
    }
    override func activate() -> NthToggle {
        counter++
        if (counter >= count_max) {
            state = !state
            counter = 0
        }
        return self
    }
}

func benchObjInst_internal(n : Int) {
  var toggle1 = Toggle(start_state: true)
  //for i in 0...5 {
  //  print(toggle1.activate().value())
  //}

  for i in 0..5 {
    var t = Toggle(start_state: true)
  }

  var ntoggle1 = NthToggle(start_state: true, max_counter: 3)
  //for i in 0...5 {
  //  print(ntoggle1.activate().value())
  //}

  for i in 0..n {
    var toggle = NthToggle(start_state: true, max_counter: 3)
  }
}

func benchObjInst() {
  let start = __mach_absolute_time__()
  benchObjInst_internal(100000000)
  let delta = __mach_absolute_time__() - start
  print("\(delta) nanoseconds.")
}


func Ackermann(M : Int, N : Int) -> Int {
  if (M == 0) { return  N + 1 }
  if (N == 0) { return Ackermann(M - 1, 1) }
  return Ackermann(M - 1, Ackermann(M, N - 1))
}

func benchAckermann() {
  let start = __mach_absolute_time__()
  print(Ackermann(3, 13))
  let delta = __mach_absolute_time__() - start
  print("\(delta) nanoseconds.")
}

func benchAll() {
  print(" --- Ackermann --------------")
  benchAckermann()
  print(" --- ObjInst ----------------")
  benchObjInst()
  print(" --- RC4 ------------------- ")
  benchRC4()
  print(" --- String Sort ------------")
  benchStringSort()
  print(" --- String Walk ------------")
  benchStringWalk()
  print(" --- String Complex Walk ----")
  benchStringComplexWalk()
  print(" --- Prim MST ----")
  print(" ----------------------------")
}

benchAll()
