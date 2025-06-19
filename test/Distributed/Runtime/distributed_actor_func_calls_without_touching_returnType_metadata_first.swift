// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name dist -target %target-swift-5.7-abi-triple -parse-as-library -j2 -parse-as-library -plugin-path %swift-plugin-dir -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out

// RUN: %target-run %t/a.out PARAMETER_TYPE
// RUN: %target-run %t/a.out RETURN_TYPE

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: OS=macosx

import Darwin
import Distributed
import Foundation

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

struct Two<T> {
  var t: (T, T)
}

struct Four<T> {
  var t: Two<Two<T>>
}

struct Sixteen<T> {
  var t: Four<Four<T>>
}

struct TwoFiftySix<T> {
  var t: Sixteen<Sixteen<T>>
}

struct TypeXXXX {
  var x: TwoFiftySix<Int>
}

struct Type1000 { var x: TwoFiftySix<Int> }
struct Type1001 { var x: TwoFiftySix<Int> }
struct Type1002 { var x: TwoFiftySix<Int> }
struct Type1003 { var x: TwoFiftySix<Int> }
struct Type1004 { var x: TwoFiftySix<Int> }
struct Type1005 { var x: TwoFiftySix<Int> }
struct Type1006 { var x: TwoFiftySix<Int> }
struct Type1007 { var x: TwoFiftySix<Int> }
struct Type1008 { var x: TwoFiftySix<Int> }
struct Type1009 { var x: TwoFiftySix<Int> }
struct Type1010 { var x: TwoFiftySix<Int> }
struct Type1011 { var x: TwoFiftySix<Int> }
struct Type1012 { var x: TwoFiftySix<Int> }
struct Type1013 { var x: TwoFiftySix<Int> }
struct Type1014 { var x: TwoFiftySix<Int> }
struct Type1015 { var x: TwoFiftySix<Int> }
struct Type1016 { var x: TwoFiftySix<Int> }
struct Type1017 { var x: TwoFiftySix<Int> }
struct Type1018 { var x: TwoFiftySix<Int> }
struct Type1019 { var x: TwoFiftySix<Int> }
struct Type1020 { var x: TwoFiftySix<Int> }
struct Type1021 { var x: TwoFiftySix<Int> }
struct Type1022 { var x: TwoFiftySix<Int> }
struct Type1023 { var x: TwoFiftySix<Int> }
struct Type1024 { var x: TwoFiftySix<Int> }
struct Type1025 { var x: TwoFiftySix<Int> }
struct Type1026 { var x: TwoFiftySix<Int> }
struct Type1027 { var x: TwoFiftySix<Int> }
struct Type1028 { var x: TwoFiftySix<Int> }
struct Type1029 { var x: TwoFiftySix<Int> }
struct Type1030 { var x: TwoFiftySix<Int> }
struct Type1031 { var x: TwoFiftySix<Int> }
struct Type1032 { var x: TwoFiftySix<Int> }
struct Type1033 { var x: TwoFiftySix<Int> }
struct Type1034 { var x: TwoFiftySix<Int> }
struct Type1035 { var x: TwoFiftySix<Int> }
struct Type1036 { var x: TwoFiftySix<Int> }
struct Type1037 { var x: TwoFiftySix<Int> }
struct Type1038 { var x: TwoFiftySix<Int> }
struct Type1039 { var x: TwoFiftySix<Int> }
struct Type1040 { var x: TwoFiftySix<Int> }
struct Type1041 { var x: TwoFiftySix<Int> }
struct Type1042 { var x: TwoFiftySix<Int> }
struct Type1043 { var x: TwoFiftySix<Int> }
struct Type1044 { var x: TwoFiftySix<Int> }
struct Type1045 { var x: TwoFiftySix<Int> }
struct Type1046 { var x: TwoFiftySix<Int> }
struct Type1047 { var x: TwoFiftySix<Int> }
struct Type1048 { var x: TwoFiftySix<Int> }
struct Type1049 { var x: TwoFiftySix<Int> }
struct Type1050 { var x: TwoFiftySix<Int> }
struct Type1051 { var x: TwoFiftySix<Int> }
struct Type1052 { var x: TwoFiftySix<Int> }
struct Type1053 { var x: TwoFiftySix<Int> }
struct Type1054 { var x: TwoFiftySix<Int> }
struct Type1055 { var x: TwoFiftySix<Int> }
struct Type1056 { var x: TwoFiftySix<Int> }
struct Type1057 { var x: TwoFiftySix<Int> }
struct Type1058 { var x: TwoFiftySix<Int> }
struct Type1059 { var x: TwoFiftySix<Int> }
struct Type1060 { var x: TwoFiftySix<Int> }
struct Type1061 { var x: TwoFiftySix<Int> }
struct Type1062 { var x: TwoFiftySix<Int> }
struct Type1063 { var x: TwoFiftySix<Int> }
struct Type1064 { var x: TwoFiftySix<Int> }
struct Type1065 { var x: TwoFiftySix<Int> }
struct Type1066 { var x: TwoFiftySix<Int> }
struct Type1067 { var x: TwoFiftySix<Int> }
struct Type1068 { var x: TwoFiftySix<Int> }
struct Type1069 { var x: TwoFiftySix<Int> }
struct Type1070 { var x: TwoFiftySix<Int> }
struct Type1071 { var x: TwoFiftySix<Int> }
struct Type1072 { var x: TwoFiftySix<Int> }
struct Type1073 { var x: TwoFiftySix<Int> }
struct Type1074 { var x: TwoFiftySix<Int> }
struct Type1075 { var x: TwoFiftySix<Int> }
struct Type1076 { var x: TwoFiftySix<Int> }
struct Type1077 { var x: TwoFiftySix<Int> }
struct Type1078 { var x: TwoFiftySix<Int> }
struct Type1079 { var x: TwoFiftySix<Int> }
struct Type1080 { var x: TwoFiftySix<Int> }
struct Type1081 { var x: TwoFiftySix<Int> }
struct Type1082 { var x: TwoFiftySix<Int> }
struct Type1083 { var x: TwoFiftySix<Int> }
struct Type1084 { var x: TwoFiftySix<Int> }
struct Type1085 { var x: TwoFiftySix<Int> }
struct Type1086 { var x: TwoFiftySix<Int> }
struct Type1087 { var x: TwoFiftySix<Int> }
struct Type1088 { var x: TwoFiftySix<Int> }
struct Type1089 { var x: TwoFiftySix<Int> }
struct Type1090 { var x: TwoFiftySix<Int> }
struct Type1091 { var x: TwoFiftySix<Int> }
struct Type1092 { var x: TwoFiftySix<Int> }
struct Type1093 { var x: TwoFiftySix<Int> }
struct Type1094 { var x: TwoFiftySix<Int> }
struct Type1095 { var x: TwoFiftySix<Int> }
struct Type1096 { var x: TwoFiftySix<Int> }
struct Type1097 { var x: TwoFiftySix<Int> }
struct Type1098 { var x: TwoFiftySix<Int> }
struct Type1099 { var x: TwoFiftySix<Int> }
struct Type1100 { var x: TwoFiftySix<Int> }
struct Type1101 { var x: TwoFiftySix<Int> }
struct Type1102 { var x: TwoFiftySix<Int> }
struct Type1103 { var x: TwoFiftySix<Int> }
struct Type1104 { var x: TwoFiftySix<Int> }
struct Type1105 { var x: TwoFiftySix<Int> }
struct Type1106 { var x: TwoFiftySix<Int> }
struct Type1107 { var x: TwoFiftySix<Int> }
struct Type1108 { var x: TwoFiftySix<Int> }
struct Type1109 { var x: TwoFiftySix<Int> }
struct Type1110 { var x: TwoFiftySix<Int> }
struct Type1111 { var x: TwoFiftySix<Int> }
struct Type1112 { var x: TwoFiftySix<Int> }
struct Type1113 { var x: TwoFiftySix<Int> }
struct Type1114 { var x: TwoFiftySix<Int> }
struct Type1115 { var x: TwoFiftySix<Int> }
struct Type1116 { var x: TwoFiftySix<Int> }
struct Type1117 { var x: TwoFiftySix<Int> }
struct Type1118 { var x: TwoFiftySix<Int> }
struct Type1119 { var x: TwoFiftySix<Int> }
struct Type1120 { var x: TwoFiftySix<Int> }
struct Type1121 { var x: TwoFiftySix<Int> }
struct Type1122 { var x: TwoFiftySix<Int> }
struct Type1123 { var x: TwoFiftySix<Int> }
struct Type1124 { var x: TwoFiftySix<Int> }
struct Type1125 { var x: TwoFiftySix<Int> }
struct Type1126 { var x: TwoFiftySix<Int> }
struct Type1127 { var x: TwoFiftySix<Int> }
struct Type1128 { var x: TwoFiftySix<Int> }
struct Type1129 { var x: TwoFiftySix<Int> }
struct Type1130 { var x: TwoFiftySix<Int> }
struct Type1131 { var x: TwoFiftySix<Int> }
struct Type1132 { var x: TwoFiftySix<Int> }
struct Type1133 { var x: TwoFiftySix<Int> }
struct Type1134 { var x: TwoFiftySix<Int> }
struct Type1135 { var x: TwoFiftySix<Int> }
struct Type1136 { var x: TwoFiftySix<Int> }
struct Type1137 { var x: TwoFiftySix<Int> }
struct Type1138 { var x: TwoFiftySix<Int> }
struct Type1139 { var x: TwoFiftySix<Int> }
struct Type1140 { var x: TwoFiftySix<Int> }
struct Type1141 { var x: TwoFiftySix<Int> }
struct Type1142 { var x: TwoFiftySix<Int> }
struct Type1143 { var x: TwoFiftySix<Int> }
struct Type1144 { var x: TwoFiftySix<Int> }
struct Type1145 { var x: TwoFiftySix<Int> }
struct Type1146 { var x: TwoFiftySix<Int> }
struct Type1147 { var x: TwoFiftySix<Int> }
struct Type1148 { var x: TwoFiftySix<Int> }
struct Type1149 { var x: TwoFiftySix<Int> }
struct Type1150 { var x: TwoFiftySix<Int> }
struct Type1151 { var x: TwoFiftySix<Int> }
struct Type1152 { var x: TwoFiftySix<Int> }
struct Type1153 { var x: TwoFiftySix<Int> }
struct Type1154 { var x: TwoFiftySix<Int> }
struct Type1155 { var x: TwoFiftySix<Int> }
struct Type1156 { var x: TwoFiftySix<Int> }
struct Type1157 { var x: TwoFiftySix<Int> }
struct Type1158 { var x: TwoFiftySix<Int> }
struct Type1159 { var x: TwoFiftySix<Int> }
struct Type1160 { var x: TwoFiftySix<Int> }
struct Type1161 { var x: TwoFiftySix<Int> }
struct Type1162 { var x: TwoFiftySix<Int> }
struct Type1163 { var x: TwoFiftySix<Int> }
struct Type1164 { var x: TwoFiftySix<Int> }
struct Type1165 { var x: TwoFiftySix<Int> }
struct Type1166 { var x: TwoFiftySix<Int> }
struct Type1167 { var x: TwoFiftySix<Int> }
struct Type1168 { var x: TwoFiftySix<Int> }
struct Type1169 { var x: TwoFiftySix<Int> }
struct Type1170 { var x: TwoFiftySix<Int> }
struct Type1171 { var x: TwoFiftySix<Int> }
struct Type1172 { var x: TwoFiftySix<Int> }
struct Type1173 { var x: TwoFiftySix<Int> }
struct Type1174 { var x: TwoFiftySix<Int> }
struct Type1175 { var x: TwoFiftySix<Int> }
struct Type1176 { var x: TwoFiftySix<Int> }
struct Type1177 { var x: TwoFiftySix<Int> }
struct Type1178 { var x: TwoFiftySix<Int> }
struct Type1179 { var x: TwoFiftySix<Int> }
struct Type1180 { var x: TwoFiftySix<Int> }
struct Type1181 { var x: TwoFiftySix<Int> }
struct Type1182 { var x: TwoFiftySix<Int> }
struct Type1183 { var x: TwoFiftySix<Int> }
struct Type1184 { var x: TwoFiftySix<Int> }
struct Type1185 { var x: TwoFiftySix<Int> }
struct Type1186 { var x: TwoFiftySix<Int> }
struct Type1187 { var x: TwoFiftySix<Int> }
struct Type1188 { var x: TwoFiftySix<Int> }
struct Type1189 { var x: TwoFiftySix<Int> }
struct Type1190 { var x: TwoFiftySix<Int> }
struct Type1191 { var x: TwoFiftySix<Int> }
struct Type1192 { var x: TwoFiftySix<Int> }
struct Type1193 { var x: TwoFiftySix<Int> }
struct Type1194 { var x: TwoFiftySix<Int> }
struct Type1195 { var x: TwoFiftySix<Int> }
struct Type1196 { var x: TwoFiftySix<Int> }
struct Type1197 { var x: TwoFiftySix<Int> }
struct Type1198 { var x: TwoFiftySix<Int> }
struct Type1199 { var x: TwoFiftySix<Int> }
struct Type1200 { var x: TwoFiftySix<Int> }
struct Type1201 { var x: TwoFiftySix<Int> }
struct Type1202 { var x: TwoFiftySix<Int> }
struct Type1203 { var x: TwoFiftySix<Int> }
struct Type1204 { var x: TwoFiftySix<Int> }
struct Type1205 { var x: TwoFiftySix<Int> }
struct Type1206 { var x: TwoFiftySix<Int> }
struct Type1207 { var x: TwoFiftySix<Int> }
struct Type1208 { var x: TwoFiftySix<Int> }
struct Type1209 { var x: TwoFiftySix<Int> }
struct Type1210 { var x: TwoFiftySix<Int> }
struct Type1211 { var x: TwoFiftySix<Int> }
struct Type1212 { var x: TwoFiftySix<Int> }
struct Type1213 { var x: TwoFiftySix<Int> }
struct Type1214 { var x: TwoFiftySix<Int> }
struct Type1215 { var x: TwoFiftySix<Int> }
struct Type1216 { var x: TwoFiftySix<Int> }
struct Type1217 { var x: TwoFiftySix<Int> }
struct Type1218 { var x: TwoFiftySix<Int> }
struct Type1219 { var x: TwoFiftySix<Int> }
struct Type1220 { var x: TwoFiftySix<Int> }
struct Type1221 { var x: TwoFiftySix<Int> }
struct Type1222 { var x: TwoFiftySix<Int> }
struct Type1223 { var x: TwoFiftySix<Int> }
struct Type1224 { var x: TwoFiftySix<Int> }
struct Type1225 { var x: TwoFiftySix<Int> }
struct Type1226 { var x: TwoFiftySix<Int> }
struct Type1227 { var x: TwoFiftySix<Int> }
struct Type1228 { var x: TwoFiftySix<Int> }
struct Type1229 { var x: TwoFiftySix<Int> }
struct Type1230 { var x: TwoFiftySix<Int> }
struct Type1231 { var x: TwoFiftySix<Int> }
struct Type1232 { var x: TwoFiftySix<Int> }
struct Type1233 { var x: TwoFiftySix<Int> }
struct Type1234 { var x: TwoFiftySix<Int> }
struct Type1235 { var x: TwoFiftySix<Int> }
struct Type1236 { var x: TwoFiftySix<Int> }
struct Type1237 { var x: TwoFiftySix<Int> }
struct Type1238 { var x: TwoFiftySix<Int> }
struct Type1239 { var x: TwoFiftySix<Int> }
struct Type1240 { var x: TwoFiftySix<Int> }
struct Type1241 { var x: TwoFiftySix<Int> }
struct Type1242 { var x: TwoFiftySix<Int> }
struct Type1243 { var x: TwoFiftySix<Int> }
struct Type1244 { var x: TwoFiftySix<Int> }
struct Type1245 { var x: TwoFiftySix<Int> }
struct Type1246 { var x: TwoFiftySix<Int> }
struct Type1247 { var x: TwoFiftySix<Int> }
struct Type1248 { var x: TwoFiftySix<Int> }
struct Type1249 { var x: TwoFiftySix<Int> }
struct Type1250 { var x: TwoFiftySix<Int> }
struct Type1251 { var x: TwoFiftySix<Int> }
struct Type1252 { var x: TwoFiftySix<Int> }
struct Type1253 { var x: TwoFiftySix<Int> }
struct Type1254 { var x: TwoFiftySix<Int> }
struct Type1255 { var x: TwoFiftySix<Int> }
struct Type1256 { var x: TwoFiftySix<Int> }
struct Type1257 { var x: TwoFiftySix<Int> }
struct Type1258 { var x: TwoFiftySix<Int> }
struct Type1259 { var x: TwoFiftySix<Int> }
struct Type1260 { var x: TwoFiftySix<Int> }
struct Type1261 { var x: TwoFiftySix<Int> }
struct Type1262 { var x: TwoFiftySix<Int> }
struct Type1263 { var x: TwoFiftySix<Int> }
struct Type1264 { var x: TwoFiftySix<Int> }
struct Type1265 { var x: TwoFiftySix<Int> }
struct Type1266 { var x: TwoFiftySix<Int> }
struct Type1267 { var x: TwoFiftySix<Int> }
struct Type1268 { var x: TwoFiftySix<Int> }
struct Type1269 { var x: TwoFiftySix<Int> }
struct Type1270 { var x: TwoFiftySix<Int> }
struct Type1271 { var x: TwoFiftySix<Int> }
struct Type1272 { var x: TwoFiftySix<Int> }
struct Type1273 { var x: TwoFiftySix<Int> }
struct Type1274 { var x: TwoFiftySix<Int> }
struct Type1275 { var x: TwoFiftySix<Int> }
struct Type1276 { var x: TwoFiftySix<Int> }
struct Type1277 { var x: TwoFiftySix<Int> }
struct Type1278 { var x: TwoFiftySix<Int> }
struct Type1279 { var x: TwoFiftySix<Int> }
struct Type1280 { var x: TwoFiftySix<Int> }
struct Type1281 { var x: TwoFiftySix<Int> }
struct Type1282 { var x: TwoFiftySix<Int> }
struct Type1283 { var x: TwoFiftySix<Int> }
struct Type1284 { var x: TwoFiftySix<Int> }
struct Type1285 { var x: TwoFiftySix<Int> }
struct Type1286 { var x: TwoFiftySix<Int> }
struct Type1287 { var x: TwoFiftySix<Int> }
struct Type1288 { var x: TwoFiftySix<Int> }
struct Type1289 { var x: TwoFiftySix<Int> }
struct Type1290 { var x: TwoFiftySix<Int> }
struct Type1291 { var x: TwoFiftySix<Int> }
struct Type1292 { var x: TwoFiftySix<Int> }
struct Type1293 { var x: TwoFiftySix<Int> }
struct Type1294 { var x: TwoFiftySix<Int> }
struct Type1295 { var x: TwoFiftySix<Int> }
struct Type1296 { var x: TwoFiftySix<Int> }
struct Type1297 { var x: TwoFiftySix<Int> }
struct Type1298 { var x: TwoFiftySix<Int> }
struct Type1299 { var x: TwoFiftySix<Int> }
struct Type1300 { var x: TwoFiftySix<Int> }
struct Type1301 { var x: TwoFiftySix<Int> }
struct Type1302 { var x: TwoFiftySix<Int> }
struct Type1303 { var x: TwoFiftySix<Int> }
struct Type1304 { var x: TwoFiftySix<Int> }
struct Type1305 { var x: TwoFiftySix<Int> }
struct Type1306 { var x: TwoFiftySix<Int> }
struct Type1307 { var x: TwoFiftySix<Int> }
struct Type1308 { var x: TwoFiftySix<Int> }
struct Type1309 { var x: TwoFiftySix<Int> }
struct Type1310 { var x: TwoFiftySix<Int> }
struct Type1311 { var x: TwoFiftySix<Int> }
struct Type1312 { var x: TwoFiftySix<Int> }
struct Type1313 { var x: TwoFiftySix<Int> }
struct Type1314 { var x: TwoFiftySix<Int> }
struct Type1315 { var x: TwoFiftySix<Int> }
struct Type1316 { var x: TwoFiftySix<Int> }
struct Type1317 { var x: TwoFiftySix<Int> }
struct Type1318 { var x: TwoFiftySix<Int> }
struct Type1319 { var x: TwoFiftySix<Int> }
struct Type1320 { var x: TwoFiftySix<Int> }
struct Type1321 { var x: TwoFiftySix<Int> }
struct Type1322 { var x: TwoFiftySix<Int> }
struct Type1323 { var x: TwoFiftySix<Int> }
struct Type1324 { var x: TwoFiftySix<Int> }
struct Type1325 { var x: TwoFiftySix<Int> }
struct Type1326 { var x: TwoFiftySix<Int> }
struct Type1327 { var x: TwoFiftySix<Int> }
struct Type1328 { var x: TwoFiftySix<Int> }
struct Type1329 { var x: TwoFiftySix<Int> }
struct Type1330 { var x: TwoFiftySix<Int> }
struct Type1331 { var x: TwoFiftySix<Int> }
struct Type1332 { var x: TwoFiftySix<Int> }
struct Type1333 { var x: TwoFiftySix<Int> }
struct Type1334 { var x: TwoFiftySix<Int> }
struct Type1335 { var x: TwoFiftySix<Int> }
struct Type1336 { var x: TwoFiftySix<Int> }
struct Type1337 { var x: TwoFiftySix<Int> }
struct Type1338 { var x: TwoFiftySix<Int> }
struct Type1339 { var x: TwoFiftySix<Int> }
struct Type1340 { var x: TwoFiftySix<Int> }
struct Type1341 { var x: TwoFiftySix<Int> }
struct Type1342 { var x: TwoFiftySix<Int> }
struct Type1343 { var x: TwoFiftySix<Int> }
struct Type1344 { var x: TwoFiftySix<Int> }
struct Type1345 { var x: TwoFiftySix<Int> }
struct Type1346 { var x: TwoFiftySix<Int> }
struct Type1347 { var x: TwoFiftySix<Int> }
struct Type1348 { var x: TwoFiftySix<Int> }
struct Type1349 { var x: TwoFiftySix<Int> }
struct Type1350 { var x: TwoFiftySix<Int> }
struct Type1351 { var x: TwoFiftySix<Int> }
struct Type1352 { var x: TwoFiftySix<Int> }
struct Type1353 { var x: TwoFiftySix<Int> }
struct Type1354 { var x: TwoFiftySix<Int> }
struct Type1355 { var x: TwoFiftySix<Int> }
struct Type1356 { var x: TwoFiftySix<Int> }
struct Type1357 { var x: TwoFiftySix<Int> }
struct Type1358 { var x: TwoFiftySix<Int> }
struct Type1359 { var x: TwoFiftySix<Int> }
struct Type1360 { var x: TwoFiftySix<Int> }
struct Type1361 { var x: TwoFiftySix<Int> }
struct Type1362 { var x: TwoFiftySix<Int> }
struct Type1363 { var x: TwoFiftySix<Int> }
struct Type1364 { var x: TwoFiftySix<Int> }
struct Type1365 { var x: TwoFiftySix<Int> }
struct Type1366 { var x: TwoFiftySix<Int> }
struct Type1367 { var x: TwoFiftySix<Int> }
struct Type1368 { var x: TwoFiftySix<Int> }
struct Type1369 { var x: TwoFiftySix<Int> }
struct Type1370 { var x: TwoFiftySix<Int> }
struct Type1371 { var x: TwoFiftySix<Int> }
struct Type1372 { var x: TwoFiftySix<Int> }
struct Type1373 { var x: TwoFiftySix<Int> }
struct Type1374 { var x: TwoFiftySix<Int> }
struct Type1375 { var x: TwoFiftySix<Int> }
struct Type1376 { var x: TwoFiftySix<Int> }
struct Type1377 { var x: TwoFiftySix<Int> }
struct Type1378 { var x: TwoFiftySix<Int> }
struct Type1379 { var x: TwoFiftySix<Int> }
struct Type1380 { var x: TwoFiftySix<Int> }
struct Type1381 { var x: TwoFiftySix<Int> }
struct Type1382 { var x: TwoFiftySix<Int> }
struct Type1383 { var x: TwoFiftySix<Int> }
struct Type1384 { var x: TwoFiftySix<Int> }
struct Type1385 { var x: TwoFiftySix<Int> }
struct Type1386 { var x: TwoFiftySix<Int> }
struct Type1387 { var x: TwoFiftySix<Int> }
struct Type1388 { var x: TwoFiftySix<Int> }
struct Type1389 { var x: TwoFiftySix<Int> }
struct Type1390 { var x: TwoFiftySix<Int> }
struct Type1391 { var x: TwoFiftySix<Int> }
struct Type1392 { var x: TwoFiftySix<Int> }
struct Type1393 { var x: TwoFiftySix<Int> }
struct Type1394 { var x: TwoFiftySix<Int> }
struct Type1395 { var x: TwoFiftySix<Int> }
struct Type1396 { var x: TwoFiftySix<Int> }
struct Type1397 { var x: TwoFiftySix<Int> }
struct Type1398 { var x: TwoFiftySix<Int> }
struct Type1399 { var x: TwoFiftySix<Int> }
struct Type1400 { var x: TwoFiftySix<Int> }
struct Type1401 { var x: TwoFiftySix<Int> }
struct Type1402 { var x: TwoFiftySix<Int> }
struct Type1403 { var x: TwoFiftySix<Int> }
struct Type1404 { var x: TwoFiftySix<Int> }
struct Type1405 { var x: TwoFiftySix<Int> }
struct Type1406 { var x: TwoFiftySix<Int> }
struct Type1407 { var x: TwoFiftySix<Int> }
struct Type1408 { var x: TwoFiftySix<Int> }
struct Type1409 { var x: TwoFiftySix<Int> }
struct Type1410 { var x: TwoFiftySix<Int> }
struct Type1411 { var x: TwoFiftySix<Int> }
struct Type1412 { var x: TwoFiftySix<Int> }
struct Type1413 { var x: TwoFiftySix<Int> }
struct Type1414 { var x: TwoFiftySix<Int> }
struct Type1415 { var x: TwoFiftySix<Int> }
struct Type1416 { var x: TwoFiftySix<Int> }
struct Type1417 { var x: TwoFiftySix<Int> }
struct Type1418 { var x: TwoFiftySix<Int> }
struct Type1419 { var x: TwoFiftySix<Int> }
struct Type1420 { var x: TwoFiftySix<Int> }
struct Type1421 { var x: TwoFiftySix<Int> }
struct Type1422 { var x: TwoFiftySix<Int> }
struct Type1423 { var x: TwoFiftySix<Int> }
struct Type1424 { var x: TwoFiftySix<Int> }
struct Type1425 { var x: TwoFiftySix<Int> }
struct Type1426 { var x: TwoFiftySix<Int> }
struct Type1427 { var x: TwoFiftySix<Int> }
struct Type1428 { var x: TwoFiftySix<Int> }
struct Type1429 { var x: TwoFiftySix<Int> }
struct Type1430 { var x: TwoFiftySix<Int> }
struct Type1431 { var x: TwoFiftySix<Int> }
struct Type1432 { var x: TwoFiftySix<Int> }
struct Type1433 { var x: TwoFiftySix<Int> }
struct Type1434 { var x: TwoFiftySix<Int> }
struct Type1435 { var x: TwoFiftySix<Int> }
struct Type1436 { var x: TwoFiftySix<Int> }
struct Type1437 { var x: TwoFiftySix<Int> }
struct Type1438 { var x: TwoFiftySix<Int> }
struct Type1439 { var x: TwoFiftySix<Int> }
struct Type1440 { var x: TwoFiftySix<Int> }
struct Type1441 { var x: TwoFiftySix<Int> }
struct Type1442 { var x: TwoFiftySix<Int> }
struct Type1443 { var x: TwoFiftySix<Int> }
struct Type1444 { var x: TwoFiftySix<Int> }
struct Type1445 { var x: TwoFiftySix<Int> }
struct Type1446 { var x: TwoFiftySix<Int> }
struct Type1447 { var x: TwoFiftySix<Int> }
struct Type1448 { var x: TwoFiftySix<Int> }
struct Type1449 { var x: TwoFiftySix<Int> }
struct Type1450 { var x: TwoFiftySix<Int> }
struct Type1451 { var x: TwoFiftySix<Int> }
struct Type1452 { var x: TwoFiftySix<Int> }
struct Type1453 { var x: TwoFiftySix<Int> }
struct Type1454 { var x: TwoFiftySix<Int> }
struct Type1455 { var x: TwoFiftySix<Int> }
struct Type1456 { var x: TwoFiftySix<Int> }
struct Type1457 { var x: TwoFiftySix<Int> }
struct Type1458 { var x: TwoFiftySix<Int> }
struct Type1459 { var x: TwoFiftySix<Int> }
struct Type1460 { var x: TwoFiftySix<Int> }
struct Type1461 { var x: TwoFiftySix<Int> }
struct Type1462 { var x: TwoFiftySix<Int> }
struct Type1463 { var x: TwoFiftySix<Int> }
struct Type1464 { var x: TwoFiftySix<Int> }
struct Type1465 { var x: TwoFiftySix<Int> }
struct Type1466 { var x: TwoFiftySix<Int> }
struct Type1467 { var x: TwoFiftySix<Int> }
struct Type1468 { var x: TwoFiftySix<Int> }
struct Type1469 { var x: TwoFiftySix<Int> }
struct Type1470 { var x: TwoFiftySix<Int> }
struct Type1471 { var x: TwoFiftySix<Int> }
struct Type1472 { var x: TwoFiftySix<Int> }
struct Type1473 { var x: TwoFiftySix<Int> }
struct Type1474 { var x: TwoFiftySix<Int> }
struct Type1475 { var x: TwoFiftySix<Int> }
struct Type1476 { var x: TwoFiftySix<Int> }
struct Type1477 { var x: TwoFiftySix<Int> }
struct Type1478 { var x: TwoFiftySix<Int> }
struct Type1479 { var x: TwoFiftySix<Int> }
struct Type1480 { var x: TwoFiftySix<Int> }
struct Type1481 { var x: TwoFiftySix<Int> }
struct Type1482 { var x: TwoFiftySix<Int> }
struct Type1483 { var x: TwoFiftySix<Int> }
struct Type1484 { var x: TwoFiftySix<Int> }
struct Type1485 { var x: TwoFiftySix<Int> }
struct Type1486 { var x: TwoFiftySix<Int> }
struct Type1487 { var x: TwoFiftySix<Int> }
struct Type1488 { var x: TwoFiftySix<Int> }
struct Type1489 { var x: TwoFiftySix<Int> }
struct Type1490 { var x: TwoFiftySix<Int> }
struct Type1491 { var x: TwoFiftySix<Int> }
struct Type1492 { var x: TwoFiftySix<Int> }
struct Type1493 { var x: TwoFiftySix<Int> }
struct Type1494 { var x: TwoFiftySix<Int> }
struct Type1495 { var x: TwoFiftySix<Int> }
struct Type1496 { var x: TwoFiftySix<Int> }
struct Type1497 { var x: TwoFiftySix<Int> }
struct Type1498 { var x: TwoFiftySix<Int> }
struct Type1499 { var x: TwoFiftySix<Int> }
struct Type1500 { var x: TwoFiftySix<Int> }
struct Type1501 { var x: TwoFiftySix<Int> }
struct Type1502 { var x: TwoFiftySix<Int> }
struct Type1503 { var x: TwoFiftySix<Int> }
struct Type1504 { var x: TwoFiftySix<Int> }
struct Type1505 { var x: TwoFiftySix<Int> }
struct Type1506 { var x: TwoFiftySix<Int> }
struct Type1507 { var x: TwoFiftySix<Int> }
struct Type1508 { var x: TwoFiftySix<Int> }
struct Type1509 { var x: TwoFiftySix<Int> }
struct Type1510 { var x: TwoFiftySix<Int> }
struct Type1511 { var x: TwoFiftySix<Int> }
struct Type1512 { var x: TwoFiftySix<Int> }
struct Type1513 { var x: TwoFiftySix<Int> }
struct Type1514 { var x: TwoFiftySix<Int> }
struct Type1515 { var x: TwoFiftySix<Int> }
struct Type1516 { var x: TwoFiftySix<Int> }
struct Type1517 { var x: TwoFiftySix<Int> }
struct Type1518 { var x: TwoFiftySix<Int> }
struct Type1519 { var x: TwoFiftySix<Int> }
struct Type1520 { var x: TwoFiftySix<Int> }
struct Type1521 { var x: TwoFiftySix<Int> }
struct Type1522 { var x: TwoFiftySix<Int> }
struct Type1523 { var x: TwoFiftySix<Int> }
struct Type1524 { var x: TwoFiftySix<Int> }
struct Type1525 { var x: TwoFiftySix<Int> }
struct Type1526 { var x: TwoFiftySix<Int> }
struct Type1527 { var x: TwoFiftySix<Int> }
struct Type1528 { var x: TwoFiftySix<Int> }
struct Type1529 { var x: TwoFiftySix<Int> }
struct Type1530 { var x: TwoFiftySix<Int> }
struct Type1531 { var x: TwoFiftySix<Int> }
struct Type1532 { var x: TwoFiftySix<Int> }
struct Type1533 { var x: TwoFiftySix<Int> }
struct Type1534 { var x: TwoFiftySix<Int> }
struct Type1535 { var x: TwoFiftySix<Int> }
struct Type1536 { var x: TwoFiftySix<Int> }
struct Type1537 { var x: TwoFiftySix<Int> }
struct Type1538 { var x: TwoFiftySix<Int> }
struct Type1539 { var x: TwoFiftySix<Int> }
struct Type1540 { var x: TwoFiftySix<Int> }
struct Type1541 { var x: TwoFiftySix<Int> }
struct Type1542 { var x: TwoFiftySix<Int> }
struct Type1543 { var x: TwoFiftySix<Int> }
struct Type1544 { var x: TwoFiftySix<Int> }
struct Type1545 { var x: TwoFiftySix<Int> }
struct Type1546 { var x: TwoFiftySix<Int> }
struct Type1547 { var x: TwoFiftySix<Int> }
struct Type1548 { var x: TwoFiftySix<Int> }
struct Type1549 { var x: TwoFiftySix<Int> }
struct Type1550 { var x: TwoFiftySix<Int> }
struct Type1551 { var x: TwoFiftySix<Int> }
struct Type1552 { var x: TwoFiftySix<Int> }
struct Type1553 { var x: TwoFiftySix<Int> }
struct Type1554 { var x: TwoFiftySix<Int> }
struct Type1555 { var x: TwoFiftySix<Int> }
struct Type1556 { var x: TwoFiftySix<Int> }
struct Type1557 { var x: TwoFiftySix<Int> }
struct Type1558 { var x: TwoFiftySix<Int> }
struct Type1559 { var x: TwoFiftySix<Int> }
struct Type1560 { var x: TwoFiftySix<Int> }
struct Type1561 { var x: TwoFiftySix<Int> }
struct Type1562 { var x: TwoFiftySix<Int> }
struct Type1563 { var x: TwoFiftySix<Int> }
struct Type1564 { var x: TwoFiftySix<Int> }
struct Type1565 { var x: TwoFiftySix<Int> }
struct Type1566 { var x: TwoFiftySix<Int> }
struct Type1567 { var x: TwoFiftySix<Int> }
struct Type1568 { var x: TwoFiftySix<Int> }
struct Type1569 { var x: TwoFiftySix<Int> }
struct Type1570 { var x: TwoFiftySix<Int> }
struct Type1571 { var x: TwoFiftySix<Int> }
struct Type1572 { var x: TwoFiftySix<Int> }
struct Type1573 { var x: TwoFiftySix<Int> }
struct Type1574 { var x: TwoFiftySix<Int> }
struct Type1575 { var x: TwoFiftySix<Int> }
struct Type1576 { var x: TwoFiftySix<Int> }
struct Type1577 { var x: TwoFiftySix<Int> }
struct Type1578 { var x: TwoFiftySix<Int> }
struct Type1579 { var x: TwoFiftySix<Int> }
struct Type1580 { var x: TwoFiftySix<Int> }
struct Type1581 { var x: TwoFiftySix<Int> }
struct Type1582 { var x: TwoFiftySix<Int> }
struct Type1583 { var x: TwoFiftySix<Int> }
struct Type1584 { var x: TwoFiftySix<Int> }
struct Type1585 { var x: TwoFiftySix<Int> }
struct Type1586 { var x: TwoFiftySix<Int> }
struct Type1587 { var x: TwoFiftySix<Int> }
struct Type1588 { var x: TwoFiftySix<Int> }
struct Type1589 { var x: TwoFiftySix<Int> }
struct Type1590 { var x: TwoFiftySix<Int> }
struct Type1591 { var x: TwoFiftySix<Int> }
struct Type1592 { var x: TwoFiftySix<Int> }
struct Type1593 { var x: TwoFiftySix<Int> }
struct Type1594 { var x: TwoFiftySix<Int> }
struct Type1595 { var x: TwoFiftySix<Int> }
struct Type1596 { var x: TwoFiftySix<Int> }
struct Type1597 { var x: TwoFiftySix<Int> }
struct Type1598 { var x: TwoFiftySix<Int> }
struct Type1599 { var x: TwoFiftySix<Int> }
struct Type1600 { var x: TwoFiftySix<Int> }
struct Type1601 { var x: TwoFiftySix<Int> }
struct Type1602 { var x: TwoFiftySix<Int> }
struct Type1603 { var x: TwoFiftySix<Int> }
struct Type1604 { var x: TwoFiftySix<Int> }
struct Type1605 { var x: TwoFiftySix<Int> }
struct Type1606 { var x: TwoFiftySix<Int> }
struct Type1607 { var x: TwoFiftySix<Int> }
struct Type1608 { var x: TwoFiftySix<Int> }
struct Type1609 { var x: TwoFiftySix<Int> }
struct Type1610 { var x: TwoFiftySix<Int> }
struct Type1611 { var x: TwoFiftySix<Int> }
struct Type1612 { var x: TwoFiftySix<Int> }
struct Type1613 { var x: TwoFiftySix<Int> }
struct Type1614 { var x: TwoFiftySix<Int> }
struct Type1615 { var x: TwoFiftySix<Int> }
struct Type1616 { var x: TwoFiftySix<Int> }
struct Type1617 { var x: TwoFiftySix<Int> }
struct Type1618 { var x: TwoFiftySix<Int> }
struct Type1619 { var x: TwoFiftySix<Int> }
struct Type1620 { var x: TwoFiftySix<Int> }
struct Type1621 { var x: TwoFiftySix<Int> }
struct Type1622 { var x: TwoFiftySix<Int> }
struct Type1623 { var x: TwoFiftySix<Int> }
struct Type1624 { var x: TwoFiftySix<Int> }
struct Type1625 { var x: TwoFiftySix<Int> }
struct Type1626 { var x: TwoFiftySix<Int> }
struct Type1627 { var x: TwoFiftySix<Int> }
struct Type1628 { var x: TwoFiftySix<Int> }
struct Type1629 { var x: TwoFiftySix<Int> }
struct Type1630 { var x: TwoFiftySix<Int> }
struct Type1631 { var x: TwoFiftySix<Int> }
struct Type1632 { var x: TwoFiftySix<Int> }
struct Type1633 { var x: TwoFiftySix<Int> }
struct Type1634 { var x: TwoFiftySix<Int> }
struct Type1635 { var x: TwoFiftySix<Int> }
struct Type1636 { var x: TwoFiftySix<Int> }
struct Type1637 { var x: TwoFiftySix<Int> }
struct Type1638 { var x: TwoFiftySix<Int> }
struct Type1639 { var x: TwoFiftySix<Int> }
struct Type1640 { var x: TwoFiftySix<Int> }
struct Type1641 { var x: TwoFiftySix<Int> }
struct Type1642 { var x: TwoFiftySix<Int> }
struct Type1643 { var x: TwoFiftySix<Int> }
struct Type1644 { var x: TwoFiftySix<Int> }
struct Type1645 { var x: TwoFiftySix<Int> }
struct Type1646 { var x: TwoFiftySix<Int> }
struct Type1647 { var x: TwoFiftySix<Int> }
struct Type1648 { var x: TwoFiftySix<Int> }
struct Type1649 { var x: TwoFiftySix<Int> }
struct Type1650 { var x: TwoFiftySix<Int> }
struct Type1651 { var x: TwoFiftySix<Int> }
struct Type1652 { var x: TwoFiftySix<Int> }
struct Type1653 { var x: TwoFiftySix<Int> }
struct Type1654 { var x: TwoFiftySix<Int> }
struct Type1655 { var x: TwoFiftySix<Int> }
struct Type1656 { var x: TwoFiftySix<Int> }
struct Type1657 { var x: TwoFiftySix<Int> }
struct Type1658 { var x: TwoFiftySix<Int> }
struct Type1659 { var x: TwoFiftySix<Int> }
struct Type1660 { var x: TwoFiftySix<Int> }
struct Type1661 { var x: TwoFiftySix<Int> }
struct Type1662 { var x: TwoFiftySix<Int> }
struct Type1663 { var x: TwoFiftySix<Int> }
struct Type1664 { var x: TwoFiftySix<Int> }
struct Type1665 { var x: TwoFiftySix<Int> }
struct Type1666 { var x: TwoFiftySix<Int> }
struct Type1667 { var x: TwoFiftySix<Int> }
struct Type1668 { var x: TwoFiftySix<Int> }
struct Type1669 { var x: TwoFiftySix<Int> }
struct Type1670 { var x: TwoFiftySix<Int> }
struct Type1671 { var x: TwoFiftySix<Int> }
struct Type1672 { var x: TwoFiftySix<Int> }
struct Type1673 { var x: TwoFiftySix<Int> }
struct Type1674 { var x: TwoFiftySix<Int> }
struct Type1675 { var x: TwoFiftySix<Int> }
struct Type1676 { var x: TwoFiftySix<Int> }
struct Type1677 { var x: TwoFiftySix<Int> }
struct Type1678 { var x: TwoFiftySix<Int> }
struct Type1679 { var x: TwoFiftySix<Int> }
struct Type1680 { var x: TwoFiftySix<Int> }
struct Type1681 { var x: TwoFiftySix<Int> }
struct Type1682 { var x: TwoFiftySix<Int> }
struct Type1683 { var x: TwoFiftySix<Int> }
struct Type1684 { var x: TwoFiftySix<Int> }
struct Type1685 { var x: TwoFiftySix<Int> }
struct Type1686 { var x: TwoFiftySix<Int> }
struct Type1687 { var x: TwoFiftySix<Int> }
struct Type1688 { var x: TwoFiftySix<Int> }
struct Type1689 { var x: TwoFiftySix<Int> }
struct Type1690 { var x: TwoFiftySix<Int> }
struct Type1691 { var x: TwoFiftySix<Int> }
struct Type1692 { var x: TwoFiftySix<Int> }
struct Type1693 { var x: TwoFiftySix<Int> }
struct Type1694 { var x: TwoFiftySix<Int> }
struct Type1695 { var x: TwoFiftySix<Int> }
struct Type1696 { var x: TwoFiftySix<Int> }
struct Type1697 { var x: TwoFiftySix<Int> }
struct Type1698 { var x: TwoFiftySix<Int> }
struct Type1699 { var x: TwoFiftySix<Int> }
struct Type1700 { var x: TwoFiftySix<Int> }
struct Type1701 { var x: TwoFiftySix<Int> }
struct Type1702 { var x: TwoFiftySix<Int> }
struct Type1703 { var x: TwoFiftySix<Int> }
struct Type1704 { var x: TwoFiftySix<Int> }
struct Type1705 { var x: TwoFiftySix<Int> }
struct Type1706 { var x: TwoFiftySix<Int> }
struct Type1707 { var x: TwoFiftySix<Int> }
struct Type1708 { var x: TwoFiftySix<Int> }
struct Type1709 { var x: TwoFiftySix<Int> }
struct Type1710 { var x: TwoFiftySix<Int> }
struct Type1711 { var x: TwoFiftySix<Int> }
struct Type1712 { var x: TwoFiftySix<Int> }
struct Type1713 { var x: TwoFiftySix<Int> }
struct Type1714 { var x: TwoFiftySix<Int> }
struct Type1715 { var x: TwoFiftySix<Int> }
struct Type1716 { var x: TwoFiftySix<Int> }
struct Type1717 { var x: TwoFiftySix<Int> }
struct Type1718 { var x: TwoFiftySix<Int> }
struct Type1719 { var x: TwoFiftySix<Int> }
struct Type1720 { var x: TwoFiftySix<Int> }
struct Type1721 { var x: TwoFiftySix<Int> }
struct Type1722 { var x: TwoFiftySix<Int> }
struct Type1723 { var x: TwoFiftySix<Int> }
struct Type1724 { var x: TwoFiftySix<Int> }
struct Type1725 { var x: TwoFiftySix<Int> }
struct Type1726 { var x: TwoFiftySix<Int> }
struct Type1727 { var x: TwoFiftySix<Int> }
struct Type1728 { var x: TwoFiftySix<Int> }
struct Type1729 { var x: TwoFiftySix<Int> }
struct Type1730 { var x: TwoFiftySix<Int> }
struct Type1731 { var x: TwoFiftySix<Int> }
struct Type1732 { var x: TwoFiftySix<Int> }
struct Type1733 { var x: TwoFiftySix<Int> }
struct Type1734 { var x: TwoFiftySix<Int> }
struct Type1735 { var x: TwoFiftySix<Int> }
struct Type1736 { var x: TwoFiftySix<Int> }
struct Type1737 { var x: TwoFiftySix<Int> }
struct Type1738 { var x: TwoFiftySix<Int> }
struct Type1739 { var x: TwoFiftySix<Int> }
struct Type1740 { var x: TwoFiftySix<Int> }
struct Type1741 { var x: TwoFiftySix<Int> }
struct Type1742 { var x: TwoFiftySix<Int> }
struct Type1743 { var x: TwoFiftySix<Int> }
struct Type1744 { var x: TwoFiftySix<Int> }
struct Type1745 { var x: TwoFiftySix<Int> }
struct Type1746 { var x: TwoFiftySix<Int> }
struct Type1747 { var x: TwoFiftySix<Int> }
struct Type1748 { var x: TwoFiftySix<Int> }
struct Type1749 { var x: TwoFiftySix<Int> }
struct Type1750 { var x: TwoFiftySix<Int> }
struct Type1751 { var x: TwoFiftySix<Int> }
struct Type1752 { var x: TwoFiftySix<Int> }
struct Type1753 { var x: TwoFiftySix<Int> }
struct Type1754 { var x: TwoFiftySix<Int> }
struct Type1755 { var x: TwoFiftySix<Int> }
struct Type1756 { var x: TwoFiftySix<Int> }
struct Type1757 { var x: TwoFiftySix<Int> }
struct Type1758 { var x: TwoFiftySix<Int> }
struct Type1759 { var x: TwoFiftySix<Int> }
struct Type1760 { var x: TwoFiftySix<Int> }
struct Type1761 { var x: TwoFiftySix<Int> }
struct Type1762 { var x: TwoFiftySix<Int> }
struct Type1763 { var x: TwoFiftySix<Int> }
struct Type1764 { var x: TwoFiftySix<Int> }
struct Type1765 { var x: TwoFiftySix<Int> }
struct Type1766 { var x: TwoFiftySix<Int> }
struct Type1767 { var x: TwoFiftySix<Int> }
struct Type1768 { var x: TwoFiftySix<Int> }
struct Type1769 { var x: TwoFiftySix<Int> }
struct Type1770 { var x: TwoFiftySix<Int> }
struct Type1771 { var x: TwoFiftySix<Int> }
struct Type1772 { var x: TwoFiftySix<Int> }
struct Type1773 { var x: TwoFiftySix<Int> }
struct Type1774 { var x: TwoFiftySix<Int> }
struct Type1775 { var x: TwoFiftySix<Int> }
struct Type1776 { var x: TwoFiftySix<Int> }
struct Type1777 { var x: TwoFiftySix<Int> }
struct Type1778 { var x: TwoFiftySix<Int> }
struct Type1779 { var x: TwoFiftySix<Int> }
struct Type1780 { var x: TwoFiftySix<Int> }
struct Type1781 { var x: TwoFiftySix<Int> }
struct Type1782 { var x: TwoFiftySix<Int> }
struct Type1783 { var x: TwoFiftySix<Int> }
struct Type1784 { var x: TwoFiftySix<Int> }
struct Type1785 { var x: TwoFiftySix<Int> }
struct Type1786 { var x: TwoFiftySix<Int> }
struct Type1787 { var x: TwoFiftySix<Int> }
struct Type1788 { var x: TwoFiftySix<Int> }
struct Type1789 { var x: TwoFiftySix<Int> }
struct Type1790 { var x: TwoFiftySix<Int> }
struct Type1791 { var x: TwoFiftySix<Int> }
struct Type1792 { var x: TwoFiftySix<Int> }
struct Type1793 { var x: TwoFiftySix<Int> }
struct Type1794 { var x: TwoFiftySix<Int> }
struct Type1795 { var x: TwoFiftySix<Int> }
struct Type1796 { var x: TwoFiftySix<Int> }
struct Type1797 { var x: TwoFiftySix<Int> }
struct Type1798 { var x: TwoFiftySix<Int> }
struct Type1799 { var x: TwoFiftySix<Int> }
struct Type1800 { var x: TwoFiftySix<Int> }
struct Type1801 { var x: TwoFiftySix<Int> }
struct Type1802 { var x: TwoFiftySix<Int> }
struct Type1803 { var x: TwoFiftySix<Int> }
struct Type1804 { var x: TwoFiftySix<Int> }
struct Type1805 { var x: TwoFiftySix<Int> }
struct Type1806 { var x: TwoFiftySix<Int> }
struct Type1807 { var x: TwoFiftySix<Int> }
struct Type1808 { var x: TwoFiftySix<Int> }
struct Type1809 { var x: TwoFiftySix<Int> }
struct Type1810 { var x: TwoFiftySix<Int> }
struct Type1811 { var x: TwoFiftySix<Int> }
struct Type1812 { var x: TwoFiftySix<Int> }
struct Type1813 { var x: TwoFiftySix<Int> }
struct Type1814 { var x: TwoFiftySix<Int> }
struct Type1815 { var x: TwoFiftySix<Int> }
struct Type1816 { var x: TwoFiftySix<Int> }
struct Type1817 { var x: TwoFiftySix<Int> }
struct Type1818 { var x: TwoFiftySix<Int> }
struct Type1819 { var x: TwoFiftySix<Int> }
struct Type1820 { var x: TwoFiftySix<Int> }
struct Type1821 { var x: TwoFiftySix<Int> }
struct Type1822 { var x: TwoFiftySix<Int> }
struct Type1823 { var x: TwoFiftySix<Int> }
struct Type1824 { var x: TwoFiftySix<Int> }
struct Type1825 { var x: TwoFiftySix<Int> }
struct Type1826 { var x: TwoFiftySix<Int> }
struct Type1827 { var x: TwoFiftySix<Int> }
struct Type1828 { var x: TwoFiftySix<Int> }
struct Type1829 { var x: TwoFiftySix<Int> }
struct Type1830 { var x: TwoFiftySix<Int> }
struct Type1831 { var x: TwoFiftySix<Int> }
struct Type1832 { var x: TwoFiftySix<Int> }
struct Type1833 { var x: TwoFiftySix<Int> }
struct Type1834 { var x: TwoFiftySix<Int> }
struct Type1835 { var x: TwoFiftySix<Int> }
struct Type1836 { var x: TwoFiftySix<Int> }
struct Type1837 { var x: TwoFiftySix<Int> }
struct Type1838 { var x: TwoFiftySix<Int> }
struct Type1839 { var x: TwoFiftySix<Int> }
struct Type1840 { var x: TwoFiftySix<Int> }
struct Type1841 { var x: TwoFiftySix<Int> }
struct Type1842 { var x: TwoFiftySix<Int> }
struct Type1843 { var x: TwoFiftySix<Int> }
struct Type1844 { var x: TwoFiftySix<Int> }
struct Type1845 { var x: TwoFiftySix<Int> }
struct Type1846 { var x: TwoFiftySix<Int> }
struct Type1847 { var x: TwoFiftySix<Int> }
struct Type1848 { var x: TwoFiftySix<Int> }
struct Type1849 { var x: TwoFiftySix<Int> }
struct Type1850 { var x: TwoFiftySix<Int> }
struct Type1851 { var x: TwoFiftySix<Int> }
struct Type1852 { var x: TwoFiftySix<Int> }
struct Type1853 { var x: TwoFiftySix<Int> }
struct Type1854 { var x: TwoFiftySix<Int> }
struct Type1855 { var x: TwoFiftySix<Int> }
struct Type1856 { var x: TwoFiftySix<Int> }
struct Type1857 { var x: TwoFiftySix<Int> }
struct Type1858 { var x: TwoFiftySix<Int> }
struct Type1859 { var x: TwoFiftySix<Int> }
struct Type1860 { var x: TwoFiftySix<Int> }
struct Type1861 { var x: TwoFiftySix<Int> }
struct Type1862 { var x: TwoFiftySix<Int> }
struct Type1863 { var x: TwoFiftySix<Int> }
struct Type1864 { var x: TwoFiftySix<Int> }
struct Type1865 { var x: TwoFiftySix<Int> }
struct Type1866 { var x: TwoFiftySix<Int> }
struct Type1867 { var x: TwoFiftySix<Int> }
struct Type1868 { var x: TwoFiftySix<Int> }
struct Type1869 { var x: TwoFiftySix<Int> }
struct Type1870 { var x: TwoFiftySix<Int> }
struct Type1871 { var x: TwoFiftySix<Int> }
struct Type1872 { var x: TwoFiftySix<Int> }
struct Type1873 { var x: TwoFiftySix<Int> }
struct Type1874 { var x: TwoFiftySix<Int> }
struct Type1875 { var x: TwoFiftySix<Int> }
struct Type1876 { var x: TwoFiftySix<Int> }
struct Type1877 { var x: TwoFiftySix<Int> }
struct Type1878 { var x: TwoFiftySix<Int> }
struct Type1879 { var x: TwoFiftySix<Int> }
struct Type1880 { var x: TwoFiftySix<Int> }
struct Type1881 { var x: TwoFiftySix<Int> }
struct Type1882 { var x: TwoFiftySix<Int> }
struct Type1883 { var x: TwoFiftySix<Int> }
struct Type1884 { var x: TwoFiftySix<Int> }
struct Type1885 { var x: TwoFiftySix<Int> }
struct Type1886 { var x: TwoFiftySix<Int> }
struct Type1887 { var x: TwoFiftySix<Int> }
struct Type1888 { var x: TwoFiftySix<Int> }
struct Type1889 { var x: TwoFiftySix<Int> }
struct Type1890 { var x: TwoFiftySix<Int> }
struct Type1891 { var x: TwoFiftySix<Int> }
struct Type1892 { var x: TwoFiftySix<Int> }
struct Type1893 { var x: TwoFiftySix<Int> }
struct Type1894 { var x: TwoFiftySix<Int> }
struct Type1895 { var x: TwoFiftySix<Int> }
struct Type1896 { var x: TwoFiftySix<Int> }
struct Type1897 { var x: TwoFiftySix<Int> }
struct Type1898 { var x: TwoFiftySix<Int> }
struct Type1899 { var x: TwoFiftySix<Int> }
struct Type1900 { var x: TwoFiftySix<Int> }
struct Type1901 { var x: TwoFiftySix<Int> }
struct Type1902 { var x: TwoFiftySix<Int> }
struct Type1903 { var x: TwoFiftySix<Int> }
struct Type1904 { var x: TwoFiftySix<Int> }
struct Type1905 { var x: TwoFiftySix<Int> }
struct Type1906 { var x: TwoFiftySix<Int> }
struct Type1907 { var x: TwoFiftySix<Int> }
struct Type1908 { var x: TwoFiftySix<Int> }
struct Type1909 { var x: TwoFiftySix<Int> }
struct Type1910 { var x: TwoFiftySix<Int> }
struct Type1911 { var x: TwoFiftySix<Int> }
struct Type1912 { var x: TwoFiftySix<Int> }
struct Type1913 { var x: TwoFiftySix<Int> }
struct Type1914 { var x: TwoFiftySix<Int> }
struct Type1915 { var x: TwoFiftySix<Int> }
struct Type1916 { var x: TwoFiftySix<Int> }
struct Type1917 { var x: TwoFiftySix<Int> }
struct Type1918 { var x: TwoFiftySix<Int> }
struct Type1919 { var x: TwoFiftySix<Int> }
struct Type1920 { var x: TwoFiftySix<Int> }
struct Type1921 { var x: TwoFiftySix<Int> }
struct Type1922 { var x: TwoFiftySix<Int> }
struct Type1923 { var x: TwoFiftySix<Int> }
struct Type1924 { var x: TwoFiftySix<Int> }
struct Type1925 { var x: TwoFiftySix<Int> }
struct Type1926 { var x: TwoFiftySix<Int> }
struct Type1927 { var x: TwoFiftySix<Int> }
struct Type1928 { var x: TwoFiftySix<Int> }
struct Type1929 { var x: TwoFiftySix<Int> }
struct Type1930 { var x: TwoFiftySix<Int> }
struct Type1931 { var x: TwoFiftySix<Int> }
struct Type1932 { var x: TwoFiftySix<Int> }
struct Type1933 { var x: TwoFiftySix<Int> }
struct Type1934 { var x: TwoFiftySix<Int> }
struct Type1935 { var x: TwoFiftySix<Int> }
struct Type1936 { var x: TwoFiftySix<Int> }
struct Type1937 { var x: TwoFiftySix<Int> }
struct Type1938 { var x: TwoFiftySix<Int> }
struct Type1939 { var x: TwoFiftySix<Int> }
struct Type1940 { var x: TwoFiftySix<Int> }
struct Type1941 { var x: TwoFiftySix<Int> }
struct Type1942 { var x: TwoFiftySix<Int> }
struct Type1943 { var x: TwoFiftySix<Int> }
struct Type1944 { var x: TwoFiftySix<Int> }
struct Type1945 { var x: TwoFiftySix<Int> }
struct Type1946 { var x: TwoFiftySix<Int> }
struct Type1947 { var x: TwoFiftySix<Int> }
struct Type1948 { var x: TwoFiftySix<Int> }
struct Type1949 { var x: TwoFiftySix<Int> }
struct Type1950 { var x: TwoFiftySix<Int> }
struct Type1951 { var x: TwoFiftySix<Int> }
struct Type1952 { var x: TwoFiftySix<Int> }
struct Type1953 { var x: TwoFiftySix<Int> }
struct Type1954 { var x: TwoFiftySix<Int> }
struct Type1955 { var x: TwoFiftySix<Int> }
struct Type1956 { var x: TwoFiftySix<Int> }
struct Type1957 { var x: TwoFiftySix<Int> }
struct Type1958 { var x: TwoFiftySix<Int> }
struct Type1959 { var x: TwoFiftySix<Int> }
struct Type1960 { var x: TwoFiftySix<Int> }
struct Type1961 { var x: TwoFiftySix<Int> }
struct Type1962 { var x: TwoFiftySix<Int> }
struct Type1963 { var x: TwoFiftySix<Int> }
struct Type1964 { var x: TwoFiftySix<Int> }
struct Type1965 { var x: TwoFiftySix<Int> }
struct Type1966 { var x: TwoFiftySix<Int> }
struct Type1967 { var x: TwoFiftySix<Int> }
struct Type1968 { var x: TwoFiftySix<Int> }
struct Type1969 { var x: TwoFiftySix<Int> }
struct Type1970 { var x: TwoFiftySix<Int> }
struct Type1971 { var x: TwoFiftySix<Int> }
struct Type1972 { var x: TwoFiftySix<Int> }
struct Type1973 { var x: TwoFiftySix<Int> }
struct Type1974 { var x: TwoFiftySix<Int> }
struct Type1975 { var x: TwoFiftySix<Int> }
struct Type1976 { var x: TwoFiftySix<Int> }
struct Type1977 { var x: TwoFiftySix<Int> }
struct Type1978 { var x: TwoFiftySix<Int> }
struct Type1979 { var x: TwoFiftySix<Int> }
struct Type1980 { var x: TwoFiftySix<Int> }
struct Type1981 { var x: TwoFiftySix<Int> }
struct Type1982 { var x: TwoFiftySix<Int> }
struct Type1983 { var x: TwoFiftySix<Int> }
struct Type1984 { var x: TwoFiftySix<Int> }
struct Type1985 { var x: TwoFiftySix<Int> }
struct Type1986 { var x: TwoFiftySix<Int> }
struct Type1987 { var x: TwoFiftySix<Int> }
struct Type1988 { var x: TwoFiftySix<Int> }
struct Type1989 { var x: TwoFiftySix<Int> }
struct Type1990 { var x: TwoFiftySix<Int> }
struct Type1991 { var x: TwoFiftySix<Int> }
struct Type1992 { var x: TwoFiftySix<Int> }
struct Type1993 { var x: TwoFiftySix<Int> }
struct Type1994 { var x: TwoFiftySix<Int> }
struct Type1995 { var x: TwoFiftySix<Int> }
struct Type1996 { var x: TwoFiftySix<Int> }
struct Type1997 { var x: TwoFiftySix<Int> }
struct Type1998 { var x: TwoFiftySix<Int> }
struct Type1999 { var x: TwoFiftySix<Int> }

struct BigGeneric<T>: Codable {
  var t: T?

  init(from: Decoder) {}
  init() {}
  func encode(to: Encoder) {}
}

distributed actor D {
  public distributed func getBigGeneric(_ value: BigGeneric<TypeXXXX>) {}
  public distributed func getBigGeneric() -> BigGeneric<TypeXXXX> {
     return BigGeneric()
   }
}

func attempt(_ mode: Mode, n: Int) {
  var funcName_returnType = "$s4dist1DC13getBigGenericAA0cD0VyAA8Type\(n)VGyF"
  var funcName_param = "$s4dist1DC13getBigGenericyyAA0cD0VyAA8Type\(n)VGF"

  func tryLookup() {
    let t: (any Any.Type)? =
      switch mode {
      case .returnType:
        funcName_returnType.withUTF8 {
          __getReturnTypeInfo($0.baseAddress!, UInt($0.count), nil, nil)
        }

      case .paramType:
        funcName_param.withUTF8 { nameBuf in
          var type: Any.Type?
          withUnsafeMutablePointer(to: &type) { typePtr in
            let ret = __getParameterTypeInfo(nameBuf.baseAddress!, UInt(nameBuf.count), nil, nil, typePtr._rawValue, 1)
            if ret != 1 {
              fatalError("Decoded \(ret) parameters, expected 1")
            }
          }
          return type
        }
      }

    guard let t else {
      print("couldn't look up type for mode: \(mode)")
      exit(1)
    }
    func examineType<T>(_t: T.Type) {
      if MemoryLayout<T>.size == 0 {
        fatalError("\(T.self) has zero size!")
      }
    }
    _openExistential(t, do: examineType)
  }

  DispatchQueue.concurrentPerform(iterations: 10) { _ in
    tryLookup()
  }
}

enum Mode: String {
  case returnType = "RETURN_TYPE"
  case paramType = "PARAMETER_TYPE"
}

@main struct Main {
  static func main() {
    if CommandLine.arguments.count < 2 {
      fatalError("Expected explicit mode in command line arguments, was: \(CommandLine.arguments)")
    }

    let mode = Mode.init(rawValue: CommandLine.arguments.dropFirst().first!)!
    print("Checking in mode: \(mode)...")

    for i in 1000...1999 {
      attempt(mode, n: i)
    }
    print("Passed in mode: \(mode)!")
  }
}
