
class MLMediaLibrary : NSObject {
  init(options options: [String : AnyObject] = [:])
  var mediaSources: [String : MLMediaSource]? { get }
}
let MLMediaSourcePhotosIdentifier: String
let MLMediaSourceiPhotoIdentifier: String
let MLMediaSourceiTunesIdentifier: String
let MLMediaSourceApertureIdentifier: String
let MLMediaSourceiMovieIdentifier: String
let MLMediaSourceFinalCutIdentifier: String
let MLMediaSourceGarageBandIdentifier: String
let MLMediaSourceLogicIdentifier: String
let MLMediaSourcePhotoBoothIdentifier: String
let MLMediaSourceCustomFoldersIdentifier: String
let MLMediaSourceMoviesFolderIdentifier: String
let MLMediaSourceAppDefinedFoldersIdentifier: String
let MLMediaLoadSourceTypesKey: String
let MLMediaLoadIncludeSourcesKey: String
let MLMediaLoadExcludeSourcesKey: String
let MLMediaLoadFoldersKey: String
let MLMediaLoadAppleLoops: String
let MLMediaLoadMoviesFolder: String
let MLMediaLoadAppFoldersKey: String
