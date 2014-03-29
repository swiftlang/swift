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
// This file contains a small number of Swift benchmarks.
// At the moment we can't serialize large chunks of the swift standard library,
// which prevents us from optimizing user code. In order to evaluate the
// performance of Swift we decided to place some benchmarks in the standard
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

var contiguousStringBenchmarkWords = [
  ContiguousString("woodshed".core),
  ContiguousString("lakism".core),
  ContiguousString("gastroperiodynia".core),
  ContiguousString("afetal".core),
  ContiguousString("ramsch".core),
  ContiguousString("Nickieben".core),
  ContiguousString("undutifulness".core),
  ContiguousString("birdglue".core),
  ContiguousString("ungentlemanize".core),
  ContiguousString("menacingly".core),
  ContiguousString("heterophile".core),
  ContiguousString("leoparde".core),
  ContiguousString("Casearia".core),
  ContiguousString("decorticate".core),
  ContiguousString("neognathic".core),
  ContiguousString("mentionable".core),
  ContiguousString("tetraphenol".core),
  ContiguousString("pseudonymal".core),
  ContiguousString("dislegitimate".core),
  ContiguousString("Discoidea".core),
  ContiguousString("intitule".core),
  ContiguousString("ionium".core),
  ContiguousString("Lotuko".core),
  ContiguousString("timbering".core),
  ContiguousString("nonliquidating".core),
  ContiguousString("oarialgia".core),
  ContiguousString("Saccobranchus".core),
  ContiguousString("reconnoiter".core),
  ContiguousString("criminative".core),
  ContiguousString("disintegratory".core),
  ContiguousString("executer".core),
  ContiguousString("Cylindrosporium".core),
  ContiguousString("complimentation".core),
  ContiguousString("Ixiama".core),
  ContiguousString("Araceae".core),
  ContiguousString("silaginoid".core),
  ContiguousString("derencephalus".core),
  ContiguousString("Lamiidae".core),
  ContiguousString("marrowlike".core),
  ContiguousString("ninepin".core),
  ContiguousString("dynastid".core),
  ContiguousString("lampfly".core),
  ContiguousString("feint".core),
  ContiguousString("trihemimer".core),
  ContiguousString("semibarbarous".core),
  ContiguousString("heresy".core),
  ContiguousString("tritanope".core),
  ContiguousString("indifferentist".core),
  ContiguousString("confound".core),
  ContiguousString("hyperbolaeon".core),
  ContiguousString("planirostral".core),
  ContiguousString("philosophunculist".core),
  ContiguousString("existence".core),
  ContiguousString("fretless".core),
  ContiguousString("Leptandra".core),
  ContiguousString("Amiranha".core),
  ContiguousString("handgravure".core),
  ContiguousString("gnash".core),
  ContiguousString("unbelievability".core),
  ContiguousString("orthotropic".core),
  ContiguousString("Susumu".core),
  ContiguousString("teleutospore".core),
  ContiguousString("sleazy".core),
  ContiguousString("shapeliness".core),
  ContiguousString("hepatotomy".core),
  ContiguousString("exclusivism".core),
  ContiguousString("stifler".core),
  ContiguousString("cunning".core),
  ContiguousString("isocyanuric".core),
  ContiguousString("pseudepigraphy".core),
  ContiguousString("carpetbagger".core),
  ContiguousString("respectiveness".core),
  ContiguousString("Jussi".core),
  ContiguousString("vasotomy".core),
  ContiguousString("proctotomy".core),
  ContiguousString("ovatotriangular".core),
  ContiguousString("aesthetic".core),
  ContiguousString("schizogamy".core),
  ContiguousString("disengagement".core),
  ContiguousString("foray".core),
  ContiguousString("haplocaulescent".core),
  ContiguousString("noncoherent".core),
  ContiguousString("astrocyte".core),
  ContiguousString("unreverberated".core),
  ContiguousString("presenile".core),
  ContiguousString("lanson".core),
  ContiguousString("enkraal".core),
  ContiguousString("contemplative".core),
  ContiguousString("Syun".core),
  ContiguousString("sartage".core),
  ContiguousString("unforgot".core),
  ContiguousString("wyde".core),
  ContiguousString("homeotransplant".core),
  ContiguousString("implicational".core),
  ContiguousString("forerunnership".core),
  ContiguousString("calcaneum".core),
  ContiguousString("stomatodeum".core),
  ContiguousString("pharmacopedia".core),
  ContiguousString("preconcessive".core),
  ContiguousString("trypanosomatic".core),
  ContiguousString("intracollegiate".core),
  ContiguousString("rampacious".core),
  ContiguousString("secundipara".core),
  ContiguousString("isomeric".core),
  ContiguousString("treehair".core),
  ContiguousString("pulmonal".core),
  ContiguousString("uvate".core),
  ContiguousString("dugway".core),
  ContiguousString("glucofrangulin".core),
  ContiguousString("unglory".core),
  ContiguousString("Amandus".core),
  ContiguousString("icterogenetic".core),
  ContiguousString("quadrireme".core),
  ContiguousString("Lagostomus".core),
  ContiguousString("brakeroot".core),
  ContiguousString("anthracemia".core),
  ContiguousString("fluted".core),
  ContiguousString("protoelastose".core),
  ContiguousString("thro".core),
  ContiguousString("pined".core),
  ContiguousString("Saxicolinae".core),
  ContiguousString("holidaymaking".core),
  ContiguousString("strigil".core),
  ContiguousString("uncurbed".core),
  ContiguousString("starling".core),
  ContiguousString("redeemeress".core),
  ContiguousString("Liliaceae".core),
  ContiguousString("imparsonee".core),
  ContiguousString("obtusish".core),
  ContiguousString("brushed".core),
  ContiguousString("mesally".core),
  ContiguousString("probosciformed".core),
  ContiguousString("Bourbonesque".core),
  ContiguousString("histological".core),
  ContiguousString("caroba".core),
  ContiguousString("digestion".core),
  ContiguousString("Vindemiatrix".core),
  ContiguousString("triactinal".core),
  ContiguousString("tattling".core),
  ContiguousString("arthrobacterium".core),
  ContiguousString("unended".core),
  ContiguousString("suspectfulness".core),
  ContiguousString("movelessness".core),
  ContiguousString("chartist".core),
  ContiguousString("Corynebacterium".core),
  ContiguousString("tercer".core),
  ContiguousString("oversaturation".core),
  ContiguousString("Congoleum".core),
  ContiguousString("antiskeptical".core),
  ContiguousString("sacral".core),
  ContiguousString("equiradiate".core),
  ContiguousString("whiskerage".core),
  ContiguousString("panidiomorphic".core),
  ContiguousString("unplanned".core),
  ContiguousString("anilopyrine".core),
  ContiguousString("Queres".core),
  ContiguousString("tartronyl".core),
  ContiguousString("Ing".core),
  ContiguousString("notehead".core),
  ContiguousString("finestiller".core),
  ContiguousString("weekender".core),
  ContiguousString("kittenhood".core),
  ContiguousString("competitrix".core),
  ContiguousString("premillenarian".core),
  ContiguousString("convergescence".core),
  ContiguousString("microcoleoptera".core),
  ContiguousString("slirt".core),
  ContiguousString("asteatosis".core),
  ContiguousString("Gruidae".core),
  ContiguousString("metastome".core),
  ContiguousString("ambuscader".core),
  ContiguousString("untugged".core),
  ContiguousString("uneducated".core),
  ContiguousString("redistill".core),
  ContiguousString("rushlight".core),
  ContiguousString("freakish".core),
  ContiguousString("dosology".core),
  ContiguousString("papyrine".core),
  ContiguousString("iconologist".core),
  ContiguousString("Bidpai".core),
  ContiguousString("prophethood".core),
  ContiguousString("pneumotropic".core),
  ContiguousString("chloroformize".core),
  ContiguousString("intemperance".core),
  ContiguousString("spongiform".core),
  ContiguousString("superindignant".core),
  ContiguousString("divider".core),
  ContiguousString("starlit".core),
  ContiguousString("merchantish".core),
  ContiguousString("indexless".core),
  ContiguousString("unidentifiably".core),
  ContiguousString("coumarone".core),
  ContiguousString("nomism".core),
  ContiguousString("diaphanous".core),
  ContiguousString("salve".core),
  ContiguousString("option".core),
  ContiguousString("anallantoic".core),
  ContiguousString("paint".core),
  ContiguousString("thiofurfuran".core),
  ContiguousString("baddeleyite".core),
  ContiguousString("Donne".core),
  ContiguousString("heterogenicity".core),
  ContiguousString("decess".core),
  ContiguousString("eschynite".core),
  ContiguousString("mamma".core),
  ContiguousString("unmonarchical".core),
  ContiguousString("Archiplata".core),
  ContiguousString("widdifow".core),
  ContiguousString("apathic".core),
  ContiguousString("overline".core),
  ContiguousString("chaetophoraceous".core),
  ContiguousString("creaky".core),
  ContiguousString("trichosporange".core),
  ContiguousString("uninterlined".core),
  ContiguousString("cometwise".core),
  ContiguousString("hermeneut".core),
  ContiguousString("unbedraggled".core),
  ContiguousString("tagged".core),
  ContiguousString("Sminthurus".core),
  ContiguousString("somniloquacious".core),
  ContiguousString("aphasiac".core),
  ContiguousString("Inoperculata".core),
  ContiguousString("photoactivity".core),
  ContiguousString("mobship".core),
  ContiguousString("unblightedly".core),
  ContiguousString("lievrite".core),
  ContiguousString("Khoja".core),
  ContiguousString("Falerian".core),
  ContiguousString("milfoil".core),
  ContiguousString("protectingly".core),
  ContiguousString("householder".core),
  ContiguousString("cathedra".core),
  ContiguousString("calmingly".core),
  ContiguousString("tordrillite".core),
  ContiguousString("rearhorse".core),
  ContiguousString("Leonard".core),
  ContiguousString("maracock".core),
  ContiguousString("youngish".core),
  ContiguousString("kammererite".core),
  ContiguousString("metanephric".core),
  ContiguousString("Sageretia".core),
  ContiguousString("diplococcoid".core),
  ContiguousString("accelerative".core),
  ContiguousString("choreal".core),
  ContiguousString("metalogical".core),
  ContiguousString("recombination".core),
  ContiguousString("unimprison".core),
  ContiguousString("invocation".core),
  ContiguousString("syndetic".core),
  ContiguousString("toadback".core),
  ContiguousString("vaned".core),
  ContiguousString("cupholder".core),
  ContiguousString("metropolitanship".core),
  ContiguousString("paramandelic".core),
  ContiguousString("dermolysis".core),
  ContiguousString("Sheriyat".core),
  ContiguousString("rhabdus".core),
  ContiguousString("seducee".core),
  ContiguousString("encrinoid".core),
  ContiguousString("unsuppliable".core),
  ContiguousString("cololite".core),
  ContiguousString("timesaver".core),
  ContiguousString("preambulate".core),
  ContiguousString("sampling".core),
  ContiguousString("roaster".core),
  ContiguousString("springald".core),
  ContiguousString("densher".core),
  ContiguousString("protraditional".core),
  ContiguousString("naturalesque".core),
  ContiguousString("Hydrodamalis".core),
  ContiguousString("cytogenic".core),
  ContiguousString("shortly".core),
  ContiguousString("cryptogrammatical".core),
  ContiguousString("squat".core),
  ContiguousString("genual".core),
  ContiguousString("backspier".core),
  ContiguousString("solubleness".core),
  ContiguousString("macroanalytical".core),
  ContiguousString("overcovetousness".core),
  ContiguousString("Natalie".core),
  ContiguousString("cuprobismutite".core),
  ContiguousString("phratriac".core),
  ContiguousString("Montanize".core),
  ContiguousString("hymnologist".core),
  ContiguousString("karyomiton".core),
  ContiguousString("podger".core),
  ContiguousString("unofficiousness".core),
  ContiguousString("antisplasher".core),
  ContiguousString("supraclavicular".core),
  ContiguousString("calidity".core),
  ContiguousString("disembellish".core),
  ContiguousString("antepredicament".core),
  ContiguousString("recurvirostral".core),
  ContiguousString("pulmonifer".core),
  ContiguousString("coccidial".core),
  ContiguousString("botonee".core),
  ContiguousString("protoglobulose".core),
  ContiguousString("isonym".core),
  ContiguousString("myeloid".core),
  ContiguousString("premiership".core),
  ContiguousString("unmonopolize".core),
  ContiguousString("unsesquipedalian".core),
  ContiguousString("unfelicitously".core),
  ContiguousString("theftbote".core),
  ContiguousString("undauntable".core),
  ContiguousString("lob".core),
  ContiguousString("praenomen".core),
  ContiguousString("underriver".core),
  ContiguousString("gorfly".core),
  ContiguousString("pluckage".core),
  ContiguousString("radiovision".core),
  ContiguousString("tyrantship".core),
  ContiguousString("fraught".core),
  ContiguousString("doppelkummel".core),
  ContiguousString("rowan".core),
  ContiguousString("allosyndetic".core),
  ContiguousString("kinesiology".core),
  ContiguousString("psychopath".core),
  ContiguousString("arrent".core),
  ContiguousString("amusively".core),
  ContiguousString("preincorporation".core),
  ContiguousString("Montargis".core),
  ContiguousString("pentacron".core),
  ContiguousString("neomedievalism".core),
  ContiguousString("sima".core),
  ContiguousString("lichenicolous".core),
  ContiguousString("Ecclesiastes".core),
  ContiguousString("woofed".core),
  ContiguousString("cardinalist".core),
  ContiguousString("sandaracin".core),
  ContiguousString("gymnasial".core),
  ContiguousString("lithoglyptics".core),
  ContiguousString("centimeter".core),
  ContiguousString("quadrupedous".core),
  ContiguousString("phraseology".core),
  ContiguousString("tumuli".core),
  ContiguousString("ankylotomy".core),
  ContiguousString("myrtol".core),
  ContiguousString("cohibitive".core),
  ContiguousString("lepospondylous".core),
  ContiguousString("silvendy".core),
  ContiguousString("inequipotential".core),
  ContiguousString("entangle".core),
  ContiguousString("raveling".core),
  ContiguousString("Zeugobranchiata".core),
  ContiguousString("devastating".core),
  ContiguousString("grainage".core),
  ContiguousString("amphisbaenian".core),
  ContiguousString("blady".core),
  ContiguousString("cirrose".core),
  ContiguousString("proclericalism".core),
  ContiguousString("governmentalist".core),
  ContiguousString("carcinomorphic".core),
  ContiguousString("nurtureship".core),
  ContiguousString("clancular".core),
  ContiguousString("unsteamed".core),
  ContiguousString("discernibly".core),
  ContiguousString("pleurogenic".core),
  ContiguousString("impalpability".core),
  ContiguousString("Azotobacterieae".core),
  ContiguousString("sarcoplasmic".core),
  ContiguousString("alternant".core),
  ContiguousString("fitly".core),
  ContiguousString("acrorrheuma".core),
  ContiguousString("shrapnel".core),
  ContiguousString("pastorize".core),
  ContiguousString("gulflike".core),
  ContiguousString("foreglow".core),
  ContiguousString("unrelated".core),
  ContiguousString("cirriped".core),
  ContiguousString("cerviconasal".core),
  ContiguousString("sexuale".core),
  ContiguousString("pussyfooter".core),
  ContiguousString("gadolinic".core),
  ContiguousString("duplicature".core),
  ContiguousString("codelinquency".core),
  ContiguousString("trypanolysis".core),
  ContiguousString("pathophobia".core),
  ContiguousString("incapsulation".core),
  ContiguousString("nonaerating".core),
  ContiguousString("feldspar".core),
  ContiguousString("diaphonic".core),
  ContiguousString("epiglottic".core),
  ContiguousString("depopulator".core),
  ContiguousString("wisecracker".core),
  ContiguousString("gravitational".core),
  ContiguousString("kuba".core),
  ContiguousString("lactesce".core),
  ContiguousString("Toxotes".core),
  ContiguousString("periomphalic".core),
  ContiguousString("singstress".core),
  ContiguousString("fannier".core),
  ContiguousString("counterformula".core),
  ContiguousString("Acemetae".core),
  ContiguousString("repugnatorial".core),
  ContiguousString("collimator".core),
  ContiguousString("Acinetina".core),
  ContiguousString("unpeace".core),
  ContiguousString("drum".core),
  ContiguousString("tetramorphic".core),
  ContiguousString("descendentalism".core),
  ContiguousString("cementer".core),
  ContiguousString("supraloral".core),
  ContiguousString("intercostal".core),
  ContiguousString("Nipponize".core),
  ContiguousString("negotiator".core),
  ContiguousString("vacationless".core),
  ContiguousString("synthol".core),
  ContiguousString("fissureless".core),
  ContiguousString("resoap".core),
  ContiguousString("pachycarpous".core),
  ContiguousString("reinspiration".core),
  ContiguousString("misappropriation".core),
  ContiguousString("disdiazo".core),
  ContiguousString("unheatable".core),
  ContiguousString("streng".core),
  ContiguousString("Detroiter".core),
  ContiguousString("infandous".core),
  ContiguousString("loganiaceous".core),
  ContiguousString("desugar".core),
  ContiguousString("Matronalia".core),
  ContiguousString("myxocystoma".core),
  ContiguousString("Gandhiism".core),
  ContiguousString("kiddier".core),
  ContiguousString("relodge".core),
  ContiguousString("counterreprisal".core),
  ContiguousString("recentralize".core),
  ContiguousString("foliously".core),
  ContiguousString("reprinter".core),
  ContiguousString("gender".core),
  ContiguousString("edaciousness".core),
  ContiguousString("chondriomite".core),
  ContiguousString("concordant".core),
  ContiguousString("stockrider".core),
  ContiguousString("pedary".core),
  ContiguousString("shikra".core),
  ContiguousString("blameworthiness".core),
  ContiguousString("vaccina".core),
  ContiguousString("Thamnophilinae".core),
  ContiguousString("wrongwise".core),
  ContiguousString("unsuperannuated".core),
  ContiguousString("convalescency".core),
  ContiguousString("intransmutable".core),
  ContiguousString("dropcloth".core),
  ContiguousString("Ceriomyces".core),
  ContiguousString("ponderal".core),
  ContiguousString("unstentorian".core),
  ContiguousString("mem".core),
  ContiguousString("deceleration".core),
  ContiguousString("ethionic".core),
  ContiguousString("untopped".core),
  ContiguousString("wetback".core),
  ContiguousString("bebar".core),
  ContiguousString("undecaying".core),
  ContiguousString("shoreside".core),
  ContiguousString("energize".core),
  ContiguousString("presacral".core),
  ContiguousString("undismay".core),
  ContiguousString("agricolite".core),
  ContiguousString("cowheart".core),
  ContiguousString("hemibathybian".core),
  ContiguousString("postexilian".core),
  ContiguousString("Phacidiaceae".core),
  ContiguousString("offing".core),
  ContiguousString("redesignation".core),
  ContiguousString("skeptically".core),
  ContiguousString("physicianless".core),
  ContiguousString("bronchopathy".core),
  ContiguousString("marabuto".core),
  ContiguousString("proprietory".core),
  ContiguousString("unobtruded".core),
  ContiguousString("funmaker".core),
  ContiguousString("plateresque".core),
  ContiguousString("preadventure".core),
  ContiguousString("beseeching".core),
  ContiguousString("cowpath".core),
  ContiguousString("pachycephalia".core),
  ContiguousString("arthresthesia".core),
  ContiguousString("supari".core),
  ContiguousString("lengthily".core),
  ContiguousString("Nepa".core),
  ContiguousString("liberation".core),
  ContiguousString("nigrify".core),
  ContiguousString("belfry".core),
  ContiguousString("entoolitic".core),
  ContiguousString("bazoo".core),
  ContiguousString("pentachromic".core),
  ContiguousString("distinguishable".core),
  ContiguousString("slideable".core),
  ContiguousString("galvanoscope".core),
  ContiguousString("remanage".core),
  ContiguousString("cetene".core),
  ContiguousString("bocardo".core),
  ContiguousString("consummation".core),
  ContiguousString("boycottism".core),
  ContiguousString("perplexity".core),
  ContiguousString("astay".core),
  ContiguousString("Gaetuli".core),
  ContiguousString("periplastic".core),
  ContiguousString("consolidator".core),
  ContiguousString("sluggarding".core),
  ContiguousString("coracoscapular".core),
  ContiguousString("anangioid".core),
  ContiguousString("oxygenizer".core),
  ContiguousString("Hunanese".core),
  ContiguousString("seminary".core),
  ContiguousString("periplast".core),
  ContiguousString("Corylus".core),
  ContiguousString("unoriginativeness".core),
  ContiguousString("persecutee".core),
  ContiguousString("tweaker".core),
  ContiguousString("silliness".core),
  ContiguousString("Dabitis".core),
  ContiguousString("facetiousness".core),
  ContiguousString("thymy".core),
  ContiguousString("nonimperial".core),
  ContiguousString("mesoblastema".core),
  ContiguousString("turbiniform".core),
  ContiguousString("churchway".core),
  ContiguousString("cooing".core),
  ContiguousString("frithbot".core),
  ContiguousString("concomitantly".core),
  ContiguousString("stalwartize".core),
  ContiguousString("clingfish".core),
  ContiguousString("hardmouthed".core),
  ContiguousString("parallelepipedonal".core),
  ContiguousString("coracoacromial".core),
  ContiguousString("factuality".core),
  ContiguousString("curtilage".core),
  ContiguousString("arachnoidean".core),
  ContiguousString("semiaridity".core),
  ContiguousString("phytobacteriology".core),
  ContiguousString("premastery".core),
  ContiguousString("hyperpurist".core),
  ContiguousString("mobed".core),
  ContiguousString("opportunistic".core),
  ContiguousString("acclimature".core),
  ContiguousString("outdistance".core),
  ContiguousString("sophister".core),
  ContiguousString("condonement".core),
  ContiguousString("oxygenerator".core),
  ContiguousString("acetonic".core),
  ContiguousString("emanatory".core),
  ContiguousString("periphlebitis".core),
  ContiguousString("nonsociety".core),
  ContiguousString("spectroradiometric".core),
  ContiguousString("superaverage".core),
  ContiguousString("cleanness".core),
  ContiguousString("posteroventral".core),
  ContiguousString("unadvised".core),
  ContiguousString("unmistakedly".core),
  ContiguousString("pimgenet".core),
  ContiguousString("auresca".core),
  ContiguousString("overimitate".core),
  ContiguousString("dipnoan".core),
  ContiguousString("chromoxylograph".core),
  ContiguousString("triakistetrahedron".core),
  ContiguousString("Suessiones".core),
  ContiguousString("uncopiable".core),
  ContiguousString("oligomenorrhea".core),
  ContiguousString("fribbling".core),
  ContiguousString("worriable".core),
  ContiguousString("flot".core),
  ContiguousString("ornithotrophy".core),
  ContiguousString("phytoteratology".core),
  ContiguousString("setup".core),
  ContiguousString("lanneret".core),
  ContiguousString("unbraceleted".core),
  ContiguousString("gudemother".core),
  ContiguousString("Spica".core),
  ContiguousString("unconsolatory".core),
  ContiguousString("recorruption".core),
  ContiguousString("premenstrual".core),
  ContiguousString("subretinal".core),
  ContiguousString("millennialist".core),
  ContiguousString("subjectibility".core),
  ContiguousString("rewardproof".core),
  ContiguousString("counterflight".core),
  ContiguousString("pilomotor".core),
  ContiguousString("carpetbaggery".core),
  ContiguousString("macrodiagonal".core),
  ContiguousString("slim".core),
  ContiguousString("indiscernible".core),
  ContiguousString("cuckoo".core),
  ContiguousString("moted".core),
  ContiguousString("controllingly".core),
  ContiguousString("gynecopathy".core),
  ContiguousString("porrectus".core),
  ContiguousString("wanworth".core),
  ContiguousString("lutfisk".core),
  ContiguousString("semiprivate".core),
  ContiguousString("philadelphy".core),
  ContiguousString("abdominothoracic".core),
  ContiguousString("coxcomb".core),
  ContiguousString("dambrod".core),
  ContiguousString("Metanemertini".core),
  ContiguousString("balminess".core),
  ContiguousString("homotypy".core),
  ContiguousString("waremaker".core),
  ContiguousString("absurdity".core),
  ContiguousString("gimcrack".core),
  ContiguousString("asquat".core),
  ContiguousString("suitable".core),
  ContiguousString("perimorphous".core),
  ContiguousString("kitchenwards".core),
  ContiguousString("pielum".core),
  ContiguousString("salloo".core),
  ContiguousString("paleontologic".core),
  ContiguousString("Olson".core),
  ContiguousString("Tellinidae".core),
  ContiguousString("ferryman".core),
  ContiguousString("peptonoid".core),
  ContiguousString("Bopyridae".core),
  ContiguousString("fallacy".core),
  ContiguousString("ictuate".core),
  ContiguousString("aguinaldo".core),
  ContiguousString("rhyodacite".core),
  ContiguousString("Ligydidae".core),
  ContiguousString("galvanometric".core),
  ContiguousString("acquisitor".core),
  ContiguousString("muscology".core),
  ContiguousString("hemikaryon".core),
  ContiguousString("ethnobotanic".core),
  ContiguousString("postganglionic".core),
  ContiguousString("rudimentarily".core),
  ContiguousString("replenish".core),
  ContiguousString("phyllorhine".core),
  ContiguousString("popgunnery".core),
  ContiguousString("summar".core),
  ContiguousString("quodlibetary".core),
  ContiguousString("xanthochromia".core),
  ContiguousString("autosymbolically".core),
  ContiguousString("preloreal".core),
  ContiguousString("extent".core),
  ContiguousString("strawberry".core),
  ContiguousString("immortalness".core),
  ContiguousString("colicwort".core),
  ContiguousString("frisca".core),
  ContiguousString("electiveness".core),
  ContiguousString("heartbroken".core),
  ContiguousString("affrightingly".core),
  ContiguousString("reconfiscation".core),
  ContiguousString("jacchus".core),
  ContiguousString("imponderably".core),
  ContiguousString("semantics".core),
  ContiguousString("beennut".core),
  ContiguousString("paleometeorological".core),
  ContiguousString("becost".core),
  ContiguousString("timberwright".core),
  ContiguousString("resuppose".core),
  ContiguousString("syncategorematical".core),
  ContiguousString("cytolymph".core),
  ContiguousString("steinbok".core),
  ContiguousString("explantation".core),
  ContiguousString("hyperelliptic".core),
  ContiguousString("antescript".core),
  ContiguousString("blowdown".core),
  ContiguousString("antinomical".core),
  ContiguousString("caravanserai".core),
  ContiguousString("unweariedly".core),
  ContiguousString("isonymic".core),
  ContiguousString("keratoplasty".core),
  ContiguousString("vipery".core),
  ContiguousString("parepigastric".core),
  ContiguousString("endolymphatic".core),
  ContiguousString("Londonese".core),
  ContiguousString("necrotomy".core),
  ContiguousString("angelship".core),
  ContiguousString("Schizogregarinida".core),
  ContiguousString("steeplebush".core),
  ContiguousString("sparaxis".core),
  ContiguousString("connectedness".core),
  ContiguousString("tolerance".core),
  ContiguousString("impingent".core),
  ContiguousString("agglutinin".core),
  ContiguousString("reviver".core),
  ContiguousString("hieroglyphical".core),
  ContiguousString("dialogize".core),
  ContiguousString("coestate".core),
  ContiguousString("declamatory".core),
  ContiguousString("ventilation".core),
  ContiguousString("tauromachy".core),
  ContiguousString("cotransubstantiate".core),
  ContiguousString("pome".core),
  ContiguousString("underseas".core),
  ContiguousString("triquadrantal".core),
  ContiguousString("preconfinemnt".core),
  ContiguousString("electroindustrial".core),
  ContiguousString("selachostomous".core),
  ContiguousString("nongolfer".core),
  ContiguousString("mesalike".core),
  ContiguousString("hamartiology".core),
  ContiguousString("ganglioblast".core),
  ContiguousString("unsuccessive".core),
  ContiguousString("yallow".core),
  ContiguousString("bacchanalianly".core),
  ContiguousString("platydactyl".core),
  ContiguousString("Bucephala".core),
  ContiguousString("ultraurgent".core),
  ContiguousString("penalist".core),
  ContiguousString("catamenial".core),
  ContiguousString("lynnhaven".core),
  ContiguousString("unrelevant".core),
  ContiguousString("lunkhead".core),
  ContiguousString("metropolitan".core),
  ContiguousString("hydro".core),
  ContiguousString("outsoar".core),
  ContiguousString("vernant".core),
  ContiguousString("interlanguage".core),
  ContiguousString("catarrhal".core),
  ContiguousString("Ionicize".core),
  ContiguousString("keelless".core),
  ContiguousString("myomantic".core),
  ContiguousString("booker".core),
  ContiguousString("Xanthomonas".core),
  ContiguousString("unimpeded".core),
  ContiguousString("overfeminize".core),
  ContiguousString("speronaro".core),
  ContiguousString("diaconia".core),
  ContiguousString("overholiness".core),
  ContiguousString("liquefacient".core),
  ContiguousString("Spartium".core),
  ContiguousString("haggly".core),
  ContiguousString("albumose".core),
  ContiguousString("nonnecessary".core),
  ContiguousString("sulcalization".core),
  ContiguousString("decapitate".core),
  ContiguousString("cellated".core),
  ContiguousString("unguirostral".core),
  ContiguousString("trichiurid".core),
  ContiguousString("loveproof".core),
  ContiguousString("amakebe".core),
  ContiguousString("screet".core),
  ContiguousString("arsenoferratin".core),
  ContiguousString("unfrizz".core),
  ContiguousString("undiscoverable".core),
  ContiguousString("procollectivistic".core),
  ContiguousString("tractile".core),
  ContiguousString("Winona".core),
  ContiguousString("dermostosis".core),
  ContiguousString("eliminant".core),
  ContiguousString("scomberoid".core),
  ContiguousString("tensile".core),
  ContiguousString("typesetting".core),
  ContiguousString("xylic".core),
  ContiguousString("dermatopathology".core),
  ContiguousString("cycloplegic".core),
  ContiguousString("revocable".core),
  ContiguousString("fissate".core),
  ContiguousString("afterplay".core),
  ContiguousString("screwship".core),
  ContiguousString("microerg".core),
  ContiguousString("bentonite".core),
  ContiguousString("stagecoaching".core),
  ContiguousString("beglerbeglic".core),
  ContiguousString("overcharitably".core),
  ContiguousString("Plotinism".core),
  ContiguousString("Veddoid".core),
  ContiguousString("disequalize".core),
  ContiguousString("cytoproct".core),
  ContiguousString("trophophore".core),
  ContiguousString("antidote".core),
  ContiguousString("allerion".core),
  ContiguousString("famous".core),
  ContiguousString("convey".core),
  ContiguousString("postotic".core),
  ContiguousString("rapillo".core),
  ContiguousString("cilectomy".core),
  ContiguousString("penkeeper".core),
  ContiguousString("patronym".core),
  ContiguousString("bravely".core),
  ContiguousString("ureteropyelitis".core),
  ContiguousString("Hildebrandine".core),
  ContiguousString("missileproof".core),
  ContiguousString("Conularia".core),
  ContiguousString("deadening".core),
  ContiguousString("Conrad".core),
  ContiguousString("pseudochylous".core),
  ContiguousString("typologically".core),
  ContiguousString("strummer".core),
  ContiguousString("luxuriousness".core),
  ContiguousString("resublimation".core),
  ContiguousString("glossiness".core),
  ContiguousString("hydrocauline".core),
  ContiguousString("anaglyph".core),
  ContiguousString("personifiable".core),
  ContiguousString("seniority".core),
  ContiguousString("formulator".core),
  ContiguousString("datiscaceous".core),
  ContiguousString("hydracrylate".core),
  ContiguousString("Tyranni".core),
  ContiguousString("Crawthumper".core),
  ContiguousString("overprove".core),
  ContiguousString("masher".core),
  ContiguousString("dissonance".core),
  ContiguousString("Serpentinian".core),
  ContiguousString("malachite".core),
  ContiguousString("interestless".core),
  ContiguousString("stchi".core),
  ContiguousString("ogum".core),
  ContiguousString("polyspermic".core),
  ContiguousString("archegoniate".core),
  ContiguousString("precogitation".core),
  ContiguousString("Alkaphrah".core),
  ContiguousString("craggily".core),
  ContiguousString("delightfulness".core),
  ContiguousString("bioplast".core),
  ContiguousString("diplocaulescent".core),
  ContiguousString("neverland".core),
  ContiguousString("interspheral".core),
  ContiguousString("chlorhydric".core),
  ContiguousString("forsakenly".core),
  ContiguousString("scandium".core),
  ContiguousString("detubation".core),
  ContiguousString("telega".core),
  ContiguousString("Valeriana".core),
  ContiguousString("centraxonial".core),
  ContiguousString("anabolite".core),
  ContiguousString("neger".core),
  ContiguousString("miscellanea".core),
  ContiguousString("whalebacker".core),
  ContiguousString("stylidiaceous".core),
  ContiguousString("unpropelled".core),
  ContiguousString("Kennedya".core),
  ContiguousString("Jacksonite".core),
  ContiguousString("ghoulish".core),
  ContiguousString("Dendrocalamus".core),
  ContiguousString("paynimhood".core),
  ContiguousString("rappist".core),
  ContiguousString("unluffed".core),
  ContiguousString("falling".core),
  ContiguousString("Lyctus".core),
  ContiguousString("uncrown".core),
  ContiguousString("warmly".core),
  ContiguousString("pneumatism".core),
  ContiguousString("Morisonian".core),
  ContiguousString("notate".core),
  ContiguousString("isoagglutinin".core),
  ContiguousString("Pelidnota".core),
  ContiguousString("previsit".core),
  ContiguousString("contradistinctly".core),
  ContiguousString("utter".core),
  ContiguousString("porometer".core),
  ContiguousString("gie".core),
  ContiguousString("germanization".core),
  ContiguousString("betwixt".core),
  ContiguousString("prenephritic".core),
  ContiguousString("underpier".core),
  ContiguousString("Eleutheria".core),
  ContiguousString("ruthenious".core),
  ContiguousString("convertor".core),
  ContiguousString("antisepsin".core),
  ContiguousString("winterage".core),
  ContiguousString("tetramethylammonium".core),
  ContiguousString("Rockaway".core),
  ContiguousString("Penaea".core),
  ContiguousString("prelatehood".core),
  ContiguousString("brisket".core),
  ContiguousString("unwishful".core),
  ContiguousString("Minahassa".core),
  ContiguousString("Briareus".core),
  ContiguousString("semiaxis".core),
  ContiguousString("disintegrant".core),
  ContiguousString("peastick".core),
  ContiguousString("iatromechanical".core),
  ContiguousString("fastus".core),
  ContiguousString("thymectomy".core),
  ContiguousString("ladyless".core),
  ContiguousString("unpreened".core),
  ContiguousString("overflutter".core),
  ContiguousString("sicker".core),
  ContiguousString("apsidally".core),
  ContiguousString("thiazine".core),
  ContiguousString("guideway".core),
  ContiguousString("pausation".core),
  ContiguousString("tellinoid".core),
  ContiguousString("abrogative".core),
  ContiguousString("foraminulate".core),
  ContiguousString("omphalos".core),
  ContiguousString("Monorhina".core),
  ContiguousString("polymyarian".core),
  ContiguousString("unhelpful".core),
  ContiguousString("newslessness".core),
  ContiguousString("oryctognosy".core),
  ContiguousString("octoradial".core),
  ContiguousString("doxology".core),
  ContiguousString("arrhythmy".core),
  ContiguousString("gugal".core),
  ContiguousString("mesityl".core),
  ContiguousString("hexaplaric".core),
  ContiguousString("Cabirian".core),
  ContiguousString("hordeiform".core),
  ContiguousString("eddyroot".core),
  ContiguousString("internarial".core),
  ContiguousString("deservingness".core),
  ContiguousString("jawbation".core),
  ContiguousString("orographically".core),
  ContiguousString("semiprecious".core),
  ContiguousString("seasick".core),
  ContiguousString("thermically".core),
  ContiguousString("grew".core),
  ContiguousString("tamability".core),
  ContiguousString("egotistically".core),
  ContiguousString("fip".core),
  ContiguousString("preabsorbent".core),
  ContiguousString("leptochroa".core),
  ContiguousString("ethnobotany".core),
  ContiguousString("podolite".core),
  ContiguousString("egoistic".core),
  ContiguousString("semitropical".core),
  ContiguousString("cero".core),
  ContiguousString("spinelessness".core),
  ContiguousString("onshore".core),
  ContiguousString("omlah".core),
  ContiguousString("tintinnabulist".core),
  ContiguousString("machila".core),
  ContiguousString("entomotomy".core),
  ContiguousString("nubile".core),
  ContiguousString("nonscholastic".core),
  ContiguousString("burnt".core),
  ContiguousString("Alea".core),
  ContiguousString("befume".core),
  ContiguousString("doctorless".core),
  ContiguousString("Napoleonic".core),
  ContiguousString("scenting".core),
  ContiguousString("apokreos".core),
  ContiguousString("cresylene".core),
  ContiguousString("paramide".core),
  ContiguousString("rattery".core),
  ContiguousString("disinterested".core),
  ContiguousString("idiopathetic".core),
  ContiguousString("negatory".core),
  ContiguousString("fervid".core),
  ContiguousString("quintato".core),
  ContiguousString("untricked".core),
  ContiguousString("Metrosideros".core),
  ContiguousString("mescaline".core),
  ContiguousString("midverse".core),
  ContiguousString("Musophagidae".core),
  ContiguousString("fictionary".core),
  ContiguousString("branchiostegous".core),
  ContiguousString("yoker".core),
  ContiguousString("residuum".core),
  ContiguousString("culmigenous".core),
  ContiguousString("fleam".core),
  ContiguousString("suffragism".core),
  ContiguousString("Anacreon".core),
  ContiguousString("sarcodous".core),
  ContiguousString("parodistic".core),
  ContiguousString("writmaking".core),
  ContiguousString("conversationism".core),
  ContiguousString("retroposed".core),
  ContiguousString("tornillo".core),
  ContiguousString("presuspect".core),
  ContiguousString("didymous".core),
  ContiguousString("Saumur".core),
  ContiguousString("spicing".core),
  ContiguousString("drawbridge".core),
  ContiguousString("cantor".core),
  ContiguousString("incumbrancer".core),
  ContiguousString("heterospory".core),
  ContiguousString("Turkeydom".core),
  ContiguousString("anteprandial".core),
  ContiguousString("neighbourship".core),
  ContiguousString("thatchless".core),
  ContiguousString("drepanoid".core),
  ContiguousString("lusher".core),
  ContiguousString("paling".core),
  ContiguousString("ecthlipsis".core),
  ContiguousString("heredosyphilitic".core),
  ContiguousString("although".core),
  ContiguousString("garetta".core),
  ContiguousString("temporarily".core),
  ContiguousString("Monotropa".core),
  ContiguousString("proglottic".core),
  ContiguousString("calyptro".core),
  ContiguousString("persiflage".core),
  ContiguousString("degradable".core),
  ContiguousString("paraspecific".core),
  ContiguousString("undecorative".core),
  ContiguousString("Pholas".core),
  ContiguousString("myelon".core),
  ContiguousString("resteal".core),
  ContiguousString("quadrantly".core),
  ContiguousString("scrimped".core),
  ContiguousString("airer".core),
  ContiguousString("deviless".core),
  ContiguousString("caliciform".core),
  ContiguousString("Sefekhet".core),
  ContiguousString("shastaite".core),
  ContiguousString("togate".core),
  ContiguousString("macrostructure".core),
  ContiguousString("bipyramid".core),
  ContiguousString("wey".core),
  ContiguousString("didynamy".core),
  ContiguousString("knacker".core),
  ContiguousString("swage".core),
  ContiguousString("supermanism".core),
  ContiguousString("epitheton".core),
  ContiguousString("overpresumptuous".core)
  ]

class RC4 {
  var State : UInt8[]
  var I : UInt8 = 0
  var J : UInt8 = 0

  init() {
    State = new UInt8[256]
  }

  func initialize(inout Key: UInt8[]) {
    for i in 0...256 {
      State[i] = UInt8(i)
    }

    var j : UInt8 = 0
    for i in 0...256 {
      var K : UInt8 = Key[i % Key.count]
      var S : UInt8 = State[i]
      j = j &+ S &+ K
      swapByIndex(i, Int(j))
    }
  }

  func swapByIndex(x: Int, y: Int) {
    let T1 : UInt8 = State[x]
    let T2 : UInt8 = State[y]
    State[x] = T2
    State[y] = T1
  }

  func next() -> UInt8 {
    I = I &+ 1
    J = J &+ State[Int(I)]
    swapByIndex(Int(I), Int(J))
    return State[Int(State[Int(I)] &+ State[Int(J)]) & 0xFF]
  }

  func encrypt(inout Data: UInt8[]) {
    var cnt = Data.count
    for i in 0...cnt {
      Data[i] = Data[i] ^ next()
    }
  }
}

func benchStringSort_internal(var words: String[]) {
  words.sort(<)
}

func benchStringSort() {
  let start = __mach_absolute_time__()
  for i in 0...50 {  // do not change '50', we have historic perf data
    // Notice that we _copy_ the array of words before we sort it.
    benchStringSort_internal(stringBenchmarkWords)
  }
  let delta = __mach_absolute_time__() - start

  println("\(delta) nanoseconds.")
  println("\(Double(delta) / Double(50)) nanoseconds/lap")
}

// Ordering function for ContiguousString. Here for the purpose of
// being able to measure the overhead of the representation of String
// in the case where we know we only have ContiguousString stored.
// *NOT* intended for general purpose use.
func lessthan(lhs: ContiguousString, rhs: ContiguousString) -> Bool {
  let lLen = lhs.count
  let rLen = rhs.count

  let minLen = lLen < rLen ? lLen : rLen
  for i in 0...minLen {
    let c1 = lhs[i]
    let c2 = rhs[i]
    if c1 < c2 {
      return true
    }
    else if c2 < c1 {
      return false
    }
  }
  return lLen < rLen
}

func benchContiguousStringSort_internal(var words: ContiguousString[]) {
  words.sort(lessthan)
}

func benchContiguousStringSort() {
  let start = __mach_absolute_time__()
  for i in 0...50 {  // do not change '50', we have historic perf data
    // Notice that we _copy_ the array of words before we sort it.
    benchContiguousStringSort_internal(contiguousStringBenchmarkWords)
  }
  let delta = __mach_absolute_time__() - start

  println("\(delta) nanoseconds.")
  println("\(Double(delta) / Double(50)) nanoseconds/lap")
}

func benchRC4_internal(messageLen : Int, iterations : Int, validate : Bool) {
  var Secret = "This is my secret message"
  var Key    = "This is my key"
  var SecretData : UInt8[] = Secret.asUTF8()
  var KeyData    : UInt8[] = Key.asUTF8()

  var LongData : UInt8[] = new UInt8[messageLen]

  // Generate a long message. 
  for i in 0...messageLen {
    LongData[i] = SecretData[i % SecretData.count]
  }

  // Generate the Encryptor and Decryptor classes.
  // FIXME: Passing KeyData to the c'tor does not type check.
  var Enc = RC4()
  var Dec = RC4()
  Enc.initialize(&KeyData)
  Dec.initialize(&KeyData)

  let start = __mach_absolute_time__()

  for i in 0...iterations {
    Enc.encrypt(&LongData)
    Dec.encrypt(&LongData)
  }

  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds.")
  println("\(Double(delta) / Double(iterations)) nanoseconds/lap")


  if (validate) {
    println("Validating ...")
    for i in 0...messageLen {
      if (LongData[i] != SecretData[i % SecretData.count]) {
        println("Error at \(i)");
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
  for i in 0...laps {
    for c in s.chars {
      count++
    }
  }
  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds.")
  println("\(Double(delta) / Double(laps)) nanoseconds/lap")
  return count
}

func benchStringWalk() -> Int {
  let s = "siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig"

  let start = __mach_absolute_time__()
  var count = 0
  let laps = 100000 // do not change this, we have historic perf data
  for i in 0...laps {
    for c in s.chars {
      count++
    }
  }
  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds.")
  println("\(Double(delta) / Double(laps)) nanoseconds/lap")
  return count
}

var _longGermanWord = "siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig"

func benchStringWalkCodeUnits() -> Int {
  var count = 0
  let start = __mach_absolute_time__()
  let laps = 100000 // do not change this, we have historic perf data
  for i in 0...laps {
    for cu in _longGermanWord.codeUnits {
      count++
    }
  }
  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds.")
  println("\(Double(delta) / Double(laps)) nanoseconds/lap")
  return count
}

var _testURL = "http://www.apple.com/iphone-5s/built-in-apps/"

func benchStringWalkURLParse() {
  let start = __mach_absolute_time__()
  let laps = 100000
  for i in 0...laps {
    var cu = _testURL.codeUnits
    var proto = ""
    var rest = ""
    let startIdx = cu.startIndex
    let endIdx = cu.endIndex
    for var j = startIdx; j < endIdx; j++ {
      if cu[j] == ":".codeUnits[0] {
        proto = String(cu[startIdx...j])
        rest = String(cu[(j + 1)...endIdx])
        break
      }
    }
  }
  let delta = __mach_absolute_time__() - start
  println("\(Double(delta) / Double(laps)) nanoseconds/lap")
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
        super.init(start_state)
        count_max = max_counter
        counter = 0
    }
    @override func activate() -> NthToggle {
        counter++
        if (counter >= count_max) {
            state = !state
            counter = 0
        }
        return self
    }
}

func benchObjInst_internal(n : Int) {
  var toggle1 = Toggle(true)
  //for i in 0...5 {
  //  println(toggle1.activate().value())
  //}

  for i in 0...5 {
    var t = Toggle(true)
  }

  var ntoggle1 = NthToggle(true, 3)
  //for i in 0...5 {
  //  println(ntoggle1.activate().value())
  //}

  for i in 0...n {
    var toggle = NthToggle(true, 3)
  }
}

func benchObjInst() {
  let start = __mach_absolute_time__()
  benchObjInst_internal(100000000)
  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds.")
}


func Ackermann(M : Int, N : Int) -> Int {
  if (M == 0) { return  N + 1 }
  if (N == 0) { return Ackermann(M - 1, 1) }
  return Ackermann(M - 1, Ackermann(M, N - 1))
}

func benchAckermann() {
  let start = __mach_absolute_time__()
  println(Ackermann(3, 13))
  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds.")
}

struct Node {
  var id : Int
  var adjList : Array<Int>

  init(i : Int) {
    id = i
    adjList = Array<Int>()
  }
}

struct NodeCost {
  var nodeId : Int
  var cost : Double
}

func getLeftChildIndex(index : Int) -> Int {
  return index*2 + 1
}

func getRightChildIndex(index : Int) -> Int {
  return (index + 1)*2
}

func getParentIndex(childIndex : Int) -> Int {
  return (childIndex - 1)/2
}

class PriorityQueue {
  var heap : Array<NodeCost?>
  var graphIndexToHeapIndexMap : Array<Int?>

  init() {
    heap = Array<NodeCost?>()
    graphIndexToHeapIndexMap = Array<Int?>()
  }

  // This should only be called when initializing an uninitialized queue.
  func append(n : NodeCost?) {
    heap.append(n)
    graphIndexToHeapIndexMap.append(n!.nodeId)
  }

  func dumpDebug() {
    println("QUEUE")
    for nodeCost in heap {
      let nodeId : Int = nodeCost!.nodeId
      let cost : Double = nodeCost!.cost
      println("(\(nodeId), \(cost))")
    }
  }

  func contains(i : Int) -> Bool {
    for nodeCost in heap {
      if nodeCost!.nodeId == i {
        return true
      }
    }
    return false
  }

  func checkInvariants(graph : Array<Node>) {
    // If the heap is empty, skip everything.
    if heap.isEmpty {
      return
    }

    var s = Array<Int>()
    s.append(0)

    while !s.isEmpty {
      let index = s.popLast();

      let leftChild  = getLeftChildIndex(index);
      let rightChild = getRightChildIndex(index);

      if (leftChild < heap.count) {
        if heap[leftChild]!.cost < heap[index]!.cost {
          println("Left: \(heap[leftChild]!.cost); Parent: \(heap[index]!.cost)")
        }
        assert(heap[leftChild]!.cost >= heap[index]!.cost);
        s.append(leftChild);
      }
      if (rightChild < heap.count) {
        if heap[rightChild]!.cost < heap[index]!.cost {
          println("Right: \(heap[rightChild]!.cost); Parent: \(heap[index]!.cost)")
        }
        assert(heap[rightChild]!.cost >= heap[index]!.cost);
        s.append(rightChild);
      }
    }

    // Make sure that each element in the graph that is not in the heap has
    // its index set to .None
    for i in 0...graph.count {
      if contains(i) {
        assert(graphIndexToHeapIndexMap[i] != .None,
          "All items contained in the heap must have an index assigned in map.")
      } else {
        assert(graphIndexToHeapIndexMap[i] == .None,
          "All items not in heap must not have an index assigned in map.")
      }
    }
  }

  // Pop off the smallest cost element, updating all data structures along the
  // way.
  func popHeap() -> NodeCost {
    // If we only have one element, just return it.
    if heap.count == 1 {
      return heap.popLast()!
    }

    // Otherwise swap the heap head with the last element of the heap and pop
    // the heap.
    swap(&heap[0], &heap[heap.count-1])
    let result = heap.popLast()

    // Invalidate the graph index of our old head and update the graph index of
    // the new head value.
    graphIndexToHeapIndexMap[heap[0]!.nodeId] = .Some(0)
    graphIndexToHeapIndexMap[result!.nodeId] = .None

    // Re-establish the heap property.
    var heapIndex = 0
    var smallestIndex : Int
    while true {
      smallestIndex = updateHeapAtIndex(heapIndex)
      if smallestIndex == heapIndex {
        break
      }
      heapIndex = smallestIndex
    }

    // Return result.
    return result!
  }

  func updateCostIfLessThan(graphIndex : Int, newCost : Double) -> Bool {
    // Look up the heap index corresponding to the input graph index.
    var heapIndex : Int? = graphIndexToHeapIndexMap[graphIndex]

    // If the graph index is not in the heap, return false.
    if heapIndex == .None {
      return false
    }

    // Otherwise, look up the cost of the node.
    let nodeCost = heap[heapIndex!]

    // If newCost >= nodeCost.1, don't update anything and return false.
    if newCost >= nodeCost!.cost {
      return false
    }

    // Ok, we know that newCost < nodeCost.1 so replace nodeCost.1 with
    // newCost and update all relevant data structures. Return true.
    heap[heapIndex!] = .Some(NodeCost(nodeCost!.nodeId, newCost))

    while heapIndex != .None && heapIndex! > 0 {
      heapIndex = .Some(getParentIndex(heapIndex!))
      updateHeapAtIndex(heapIndex!)
    }

    return true
  }


  func updateHeapAtIndex(index : Int) -> Int {
    let leftChildIndex : Int = getLeftChildIndex(index)
    let rightChildIndex : Int = getRightChildIndex(index)

    var smallestIndex : Int = 0
    if leftChildIndex < heap.count &&
       heap[leftChildIndex]!.cost < heap[index]!.cost {
      smallestIndex = leftChildIndex
    } else {
      smallestIndex = index
    }

    if rightChildIndex < heap.count &&
       heap[rightChildIndex]!.cost < heap[smallestIndex]!.cost {
      smallestIndex = rightChildIndex
    }

    if smallestIndex != index {
      swap(&graphIndexToHeapIndexMap[heap[index]!.nodeId],
           &graphIndexToHeapIndexMap[heap[smallestIndex]!.nodeId])
      swap(&heap[index], &heap[smallestIndex])
    }

    return smallestIndex
  }

  func isEmpty() -> Bool {
    return heap.isEmpty
  }
}

func prim(
  graph : Array<Node>,
  fun : (Int, Int) -> Double) -> Array<Int?> {

  var treeEdges = Array<Int?>()

  // Create our queue, selecting the first element of the grpah as the root of
  // our tree for simplciity.
  var queue = PriorityQueue()
  queue.append(.Some(NodeCost(0, 0.0)))

  // Make the minimum spanning tree root its own parent for simplicity.
  treeEdges.append(.Some(0))

  // Create the graph.
  for i in 1...graph.count {
    queue.append(.Some(NodeCost(i, Double.inf())))
    treeEdges.append(.None)
  }

  while !queue.isEmpty() {
    let e = queue.popHeap()
    let nodeId = e.nodeId
    for adjNodeIndex in graph[nodeId].adjList {
      if queue.updateCostIfLessThan(adjNodeIndex, fun(graph[nodeId].id,
                                                      graph[adjNodeIndex].id)) {
        treeEdges[adjNodeIndex] = .Some(nodeId)
      }
    }
  }

  return treeEdges
}

struct Edge : Equatable {
  var start : Int
  var end : Int
}

func ==(lhs: Edge, rhs: Edge) -> Bool {
  return lhs.start == rhs.start && lhs.end == rhs.end
}

extension Edge : Hashable {
  func hashValue() -> Int {
    return start.hashValue() ^ end.hashValue()
  }
}

func benchPrimsInternal(iterations: Int) {
  for i in 0...iterations {
    var graph = Array<Node>()

    var nodes : Int[] = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
      29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,
      45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
      61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
      77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92,
      93, 94, 95, 96, 97, 98, 99 ]

    var edges : (Int, Int, Double)[] = [
      (26, 47, 921),
      (20, 25, 971),
      (92, 59, 250),
      (33, 55, 1391),
      (78, 39, 313),
      (7, 25, 637),
      (18, 19, 1817),
      (33, 41, 993),
      (64, 41, 926),
      (88, 86, 574),
      (93, 15, 1462),
      (86, 33, 1649),
      (37, 35, 841),
      (98, 51, 1160),
      (15, 30, 1125),
      (65, 78, 1052),
      (58, 12, 1273),
      (12, 17, 285),
      (45, 61, 1608),
      (75, 53, 545),
      (99, 48, 410),
      (97, 0, 1303),
      (48, 17, 1807),
      (1, 54, 1491),
      (15, 34, 807),
      (94, 98, 646),
      (12, 69, 136),
      (65, 11, 983),
      (63, 83, 1604),
      (78, 89, 1828),
      (61, 63, 845),
      (18, 36, 1626),
      (68, 52, 1324),
      (14, 50, 690),
      (3, 11, 943),
      (21, 68, 914),
      (19, 44, 1762),
      (85, 80, 270),
      (59, 92, 250),
      (86, 84, 1431),
      (19, 18, 1817),
      (52, 68, 1324),
      (16, 29, 1108),
      (36, 80, 395),
      (67, 18, 803),
      (63, 88, 1717),
      (68, 21, 914),
      (75, 82, 306),
      (49, 82, 1292),
      (73, 45, 1876),
      (89, 82, 409),
      (45, 47, 272),
      (22, 83, 597),
      (61, 12, 1791),
      (44, 68, 1229),
      (50, 51, 917),
      (14, 53, 355),
      (77, 41, 138),
      (54, 21, 1870),
      (93, 70, 1582),
      (76, 2, 1658),
      (83, 73, 1162),
      (6, 1, 482),
      (11, 65, 983),
      (81, 90, 1024),
      (19, 1, 970),
      (8, 58, 1131),
      (60, 42, 477),
      (86, 29, 258),
      (69, 59, 903),
      (34, 15, 807),
      (37, 2, 1451),
      (7, 73, 754),
      (47, 86, 184),
      (67, 17, 449),
      (18, 67, 803),
      (25, 4, 595),
      (3, 31, 1337),
      (64, 31, 1928),
      (9, 43, 237),
      (83, 63, 1604),
      (47, 45, 272),
      (86, 88, 574),
      (87, 74, 934),
      (98, 94, 646),
      (20, 1, 642),
      (26, 92, 1344),
      (18, 17, 565),
      (47, 11, 595),
      (10, 59, 1558),
      (2, 76, 1658),
      (77, 74, 1277),
      (42, 60, 477),
      (80, 36, 395),
      (35, 23, 589),
      (50, 37, 203),
      (6, 96, 481),
      (78, 65, 1052),
      (1, 52, 127),
      (65, 23, 1932),
      (46, 51, 213),
      (59, 89, 89),
      (15, 93, 1462),
      (69, 3, 1305),
      (17, 37, 1177),
      (30, 3, 193),
      (9, 15, 818),
      (75, 95, 977),
      (86, 47, 184),
      (10, 12, 1736),
      (80, 27, 1010),
      (12, 10, 1736),
      (86, 1, 1958),
      (60, 12, 1240),
      (43, 71, 683),
      (91, 65, 1519),
      (33, 86, 1649),
      (62, 26, 1773),
      (1, 13, 1187),
      (2, 10, 1018),
      (91, 29, 351),
      (69, 12, 136),
      (43, 9, 237),
      (29, 86, 258),
      (17, 48, 1807),
      (31, 64, 1928),
      (68, 61, 1936),
      (76, 38, 1724),
      (1, 6, 482),
      (53, 14, 355),
      (51, 50, 917),
      (54, 13, 815),
      (19, 29, 883),
      (35, 87, 974),
      (70, 96, 511),
      (23, 35, 589),
      (39, 69, 1588),
      (93, 73, 1093),
      (13, 73, 435),
      (5, 60, 1619),
      (42, 41, 1523),
      (66, 58, 1596),
      (1, 67, 431),
      (17, 67, 449),
      (30, 95, 906),
      (71, 43, 683),
      (5, 87, 190),
      (12, 78, 891),
      (30, 97, 402),
      (28, 17, 1131),
      (7, 97, 1356),
      (58, 66, 1596),
      (20, 37, 1294),
      (73, 76, 514),
      (54, 8, 613),
      (68, 35, 1252),
      (92, 32, 701),
      (3, 90, 652),
      (99, 46, 1576),
      (13, 54, 815),
      (20, 87, 1390),
      (36, 18, 1626),
      (51, 26, 1146),
      (2, 23, 581),
      (29, 7, 1558),
      (88, 59, 173),
      (17, 1, 1071),
      (37, 49, 1011),
      (18, 6, 696),
      (88, 33, 225),
      (58, 38, 802),
      (87, 50, 1744),
      (29, 91, 351),
      (6, 71, 1053),
      (45, 24, 1720),
      (65, 91, 1519),
      (37, 50, 203),
      (11, 3, 943),
      (72, 65, 1330),
      (45, 50, 339),
      (25, 20, 971),
      (15, 9, 818),
      (14, 54, 1353),
      (69, 95, 393),
      (8, 66, 1213),
      (52, 2, 1608),
      (50, 14, 690),
      (50, 45, 339),
      (1, 37, 1273),
      (45, 93, 1650),
      (39, 78, 313),
      (1, 86, 1958),
      (17, 28, 1131),
      (35, 33, 1667),
      (23, 2, 581),
      (51, 66, 245),
      (17, 54, 924),
      (41, 49, 1629),
      (60, 5, 1619),
      (56, 93, 1110),
      (96, 13, 461),
      (25, 7, 637),
      (11, 69, 370),
      (90, 3, 652),
      (39, 71, 1485),
      (65, 51, 1529),
      (20, 6, 1414),
      (80, 85, 270),
      (73, 83, 1162),
      (0, 97, 1303),
      (13, 33, 826),
      (29, 71, 1788),
      (33, 12, 461),
      (12, 58, 1273),
      (69, 39, 1588),
      (67, 75, 1504),
      (87, 20, 1390),
      (88, 97, 526),
      (33, 88, 225),
      (95, 69, 393),
      (2, 52, 1608),
      (5, 25, 719),
      (34, 78, 510),
      (53, 99, 1074),
      (33, 35, 1667),
      (57, 30, 361),
      (87, 58, 1574),
      (13, 90, 1030),
      (79, 74, 91),
      (4, 86, 1107),
      (64, 94, 1609),
      (11, 12, 167),
      (30, 45, 272),
      (47, 91, 561),
      (37, 17, 1177),
      (77, 49, 883),
      (88, 23, 1747),
      (70, 80, 995),
      (62, 77, 907),
      (18, 4, 371),
      (73, 93, 1093),
      (11, 47, 595),
      (44, 23, 1990),
      (20, 0, 512),
      (3, 69, 1305),
      (82, 3, 1815),
      (20, 88, 368),
      (44, 45, 364),
      (26, 51, 1146),
      (7, 65, 349),
      (71, 39, 1485),
      (56, 88, 1954),
      (94, 69, 1397),
      (12, 28, 544),
      (95, 75, 977),
      (32, 90, 789),
      (53, 1, 772),
      (54, 14, 1353),
      (49, 77, 883),
      (92, 26, 1344),
      (17, 18, 565),
      (97, 88, 526),
      (48, 80, 1203),
      (90, 32, 789),
      (71, 6, 1053),
      (87, 35, 974),
      (55, 90, 1808),
      (12, 61, 1791),
      (1, 96, 328),
      (63, 10, 1681),
      (76, 34, 871),
      (41, 64, 926),
      (42, 97, 482),
      (25, 5, 719),
      (23, 65, 1932),
      (54, 1, 1491),
      (28, 12, 544),
      (89, 10, 108),
      (27, 33, 143),
      (67, 1, 431),
      (32, 45, 52),
      (79, 33, 1871),
      (6, 55, 717),
      (10, 58, 459),
      (67, 39, 393),
      (10, 4, 1808),
      (96, 6, 481),
      (1, 19, 970),
      (97, 7, 1356),
      (29, 16, 1108),
      (1, 53, 772),
      (30, 15, 1125),
      (4, 6, 634),
      (6, 20, 1414),
      (88, 56, 1954),
      (87, 64, 1950),
      (34, 76, 871),
      (17, 12, 285),
      (55, 59, 321),
      (61, 68, 1936),
      (50, 87, 1744),
      (84, 44, 952),
      (41, 33, 993),
      (59, 18, 1352),
      (33, 27, 143),
      (38, 32, 1210),
      (55, 70, 1264),
      (38, 58, 802),
      (1, 20, 642),
      (73, 13, 435),
      (80, 48, 1203),
      (94, 64, 1609),
      (38, 28, 414),
      (73, 23, 1113),
      (78, 12, 891),
      (26, 62, 1773),
      (87, 43, 579),
      (53, 6, 95),
      (59, 95, 285),
      (88, 63, 1717),
      (17, 5, 633),
      (66, 8, 1213),
      (41, 42, 1523),
      (83, 22, 597),
      (95, 30, 906),
      (51, 65, 1529),
      (17, 49, 1727),
      (64, 87, 1950),
      (86, 4, 1107),
      (37, 98, 1102),
      (32, 92, 701),
      (60, 94, 198),
      (73, 98, 1749),
      (4, 18, 371),
      (96, 70, 511),
      (7, 29, 1558),
      (35, 37, 841),
      (27, 64, 384),
      (12, 33, 461),
      (36, 38, 529),
      (69, 16, 1183),
      (91, 47, 561),
      (85, 29, 1676),
      (3, 82, 1815),
      (69, 58, 1579),
      (93, 45, 1650),
      (97, 42, 482),
      (37, 1, 1273),
      (61, 4, 543),
      (96, 1, 328),
      (26, 0, 1993),
      (70, 64, 878),
      (3, 30, 193),
      (58, 69, 1579),
      (4, 25, 595),
      (31, 3, 1337),
      (55, 6, 717),
      (39, 67, 393),
      (78, 34, 510),
      (75, 67, 1504),
      (6, 53, 95),
      (51, 79, 175),
      (28, 91, 1040),
      (89, 78, 1828),
      (74, 93, 1587),
      (45, 32, 52),
      (10, 2, 1018),
      (49, 37, 1011),
      (63, 61, 845),
      (0, 20, 512),
      (1, 17, 1071),
      (99, 53, 1074),
      (37, 20, 1294),
      (10, 89, 108),
      (33, 92, 946),
      (23, 73, 1113),
      (23, 88, 1747),
      (49, 17, 1727),
      (88, 20, 368),
      (21, 54, 1870),
      (70, 93, 1582),
      (59, 88, 173),
      (32, 38, 1210),
      (89, 59, 89),
      (23, 44, 1990),
      (38, 76, 1724),
      (30, 57, 361),
      (94, 60, 198),
      (59, 10, 1558),
      (55, 64, 1996),
      (12, 11, 167),
      (36, 24, 1801),
      (97, 30, 402),
      (52, 1, 127),
      (58, 87, 1574),
      (54, 17, 924),
      (93, 74, 1587),
      (24, 36, 1801),
      (2, 37, 1451),
      (91, 28, 1040),
      (59, 55, 321),
      (69, 11, 370),
      (8, 54, 613),
      (29, 85, 1676),
      (44, 19, 1762),
      (74, 79, 91),
      (93, 56, 1110),
      (58, 10, 459),
      (41, 50, 1559),
      (66, 51, 245),
      (80, 19, 1838),
      (33, 79, 1871),
      (76, 73, 514),
      (98, 37, 1102),
      (45, 44, 364),
      (16, 69, 1183),
      (49, 41, 1629),
      (19, 80, 1838),
      (71, 57, 500),
      (6, 4, 634),
      (64, 27, 384),
      (84, 86, 1431),
      (5, 17, 633),
      (96, 88, 334),
      (87, 5, 190),
      (70, 21, 1619),
      (55, 33, 1391),
      (10, 63, 1681),
      (11, 62, 1339),
      (33, 13, 826),
      (64, 70, 878),
      (65, 72, 1330),
      (70, 55, 1264),
      (64, 55, 1996),
      (50, 41, 1559),
      (46, 99, 1576),
      (88, 96, 334),
      (51, 20, 868),
      (73, 7, 754),
      (80, 70, 995),
      (44, 84, 952),
      (29, 19, 883),
      (59, 69, 903),
      (57, 53, 1575),
      (90, 13, 1030),
      (28, 38, 414),
      (12, 60, 1240),
      (85, 58, 573),
      (90, 55, 1808),
      (4, 10, 1808),
      (68, 44, 1229),
      (92, 33, 946),
      (90, 81, 1024),
      (53, 75, 545),
      (45, 30, 272),
      (41, 77, 138),
      (21, 70, 1619),
      (45, 73, 1876),
      (35, 68, 1252),
      (13, 96, 461),
      (53, 57, 1575),
      (82, 89, 409),
      (28, 61, 449),
      (58, 61, 78),
      (27, 80, 1010),
      (61, 58, 78),
      (38, 36, 529),
      (80, 30, 397),
      (18, 59, 1352),
      (62, 11, 1339),
      (95, 59, 285),
      (51, 98, 1160),
      (6, 18, 696),
      (30, 80, 397),
      (69, 94, 1397),
      (58, 85, 573),
      (48, 99, 410),
      (51, 46, 213),
      (57, 71, 500),
      (91, 30, 104),
      (65, 7, 349),
      (79, 51, 175),
      (47, 26, 921),
      (4, 61, 543),
      (98, 73, 1749),
      (74, 77, 1277),
      (61, 28, 449),
      (58, 8, 1131),
      (61, 45, 1608),
      (74, 87, 934),
      (71, 29, 1788),
      (30, 91, 104),
      (13, 1, 1187),
      (0, 26, 1993),
      (82, 49, 1292),
      (43, 87, 579),
      (24, 45, 1720),
      (20, 51, 868),
      (77, 62, 907),
      (82, 75, 306),
    ]

    for n in nodes {
      graph.append(Node(n))
    }

    var map = Dictionary<Edge, Double>()
    for tup in edges {
      map.add(Edge(tup.0, tup.1), tup.2)
      graph[tup.0].adjList.append(tup.1)
    }

    let treeEdges = prim(graph, { (start: Int, end: Int) in
        return map[Edge(start, end)]
    })

    //for i in 0...treeEdges.count {
    //  println("Node: \(i). Parent: \(treeEdges[i]!)")
    //}
  }
}

func benchPrims() {
  let start = __mach_absolute_time__()
  benchPrimsInternal(100)
  let delta = __mach_absolute_time__() - start
  println("\(delta) nanoseconds.")
}

func benchAll() {
  println(" --- Ackermann --------------")
  benchAckermann()
  println(" --- ObjInst ----------------")
  benchObjInst()
  println(" --- RC4 ------------------- ")
  benchRC4()
  println(" --- String Sort ------------")
  benchStringSort()
  println(" --- String Walk ------------")
  benchStringWalk()
  println(" --- String Walk Code Units -")
  benchStringWalkCodeUnits()
  println(" --- String Walk URL Parse --")
  benchStringWalkURLParse()
  println(" --- String Complex Walk ----")
  benchStringComplexWalk()
  println(" --- Prim MST ----")
  benchPrims()
  println(" ----------------------------")
}
