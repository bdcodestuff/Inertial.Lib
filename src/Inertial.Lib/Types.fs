namespace Inertial.Lib

open Thoth.Json
open System

[<AutoOpen>]
module Types =

  type AsyncChoice<'T> = Choice<Async<'T>,'T>
  type AsyncDataDecoder<'T> =
    | Choice2OptionDecoder of Decoder<AsyncChoice<Option<'T>>>
    | Choice2OptionListDecoder of Decoder<AsyncChoice<Option<'T list>>>
    | Choice2ResultListDecoder of Decoder<AsyncChoice<Result<'T list,string>>>
    | Choice2ListDecoder of Decoder<AsyncChoice<List<'T>>>
  
  type AsyncDataName=
    | Choice2Option
    | Choice2OptionList
    | Choice2ResultList
    | Choice2List
    member x.ToName() =
      match x with
      | Choice2Option -> "Choice2Option"
      | Choice2OptionList -> "Choice2OptionList" 
      | Choice2ResultList -> "Choice2ResultList"
      | Choice2List -> "Choice2List"
  
  type AsyncData<'T> =
    | Choice2Option of AsyncChoice<Option<'T>> //AsyncChoice<Option<'T>>
    | Choice2OptionList of AsyncChoice<Option<'T list>> //AsyncChoice<Option<'T list>>
    | Choice2ResultList of AsyncChoice<Result<'T list,string>> //AsyncChoice<Result<'T list,string>>
    | Choice2List of AsyncChoice<'T list> //AsyncChoice<List<'T>> 
  
  
  type ScrollPosition =
      | ResetScroll
      | KeepVerticalScroll of string
      
  type ScrollRegion =
    {
      top : double
      left : double
    }
    static member encoder = Encode.Auto.generateEncoder<ScrollRegion>()
    static member decoder = Decode.Auto.generateDecoder<ScrollRegion>()

  type CacheStorage =
    | StoreToCache of string array
    | StoreAll
    | StoreNone // no caching
    member x.ToHeader() =
      match x with
      | StoreToCache arr ->
        let arrStr =
          match arr with
          | [||] -> failwith "StoreToCache requires an array of field names to store"
          | arr ->  arr |> Array.reduce (fun x y -> $"{x},{y}")
        $"StoreToCache {arrStr}"
      | StoreAll -> "StoreAll"
      | StoreNone -> "StoreNone"
    static member decoder =
      let decodeStoreNone =
          Decode.string
            |> Decode.andThen
            (function
              | "StoreNone" -> Decode.succeed StoreNone
              | a -> Decode.fail $"Cannot decode Cache Storage from given input: {a}")
      
      let decodeStoreAll =
          Decode.string
            |> Decode.andThen
            (function
              | "StoreAll" -> Decode.succeed StoreAll
              | a -> Decode.fail $"Cannot decode Cache Storage from given input: {a}")
      
      let decodeStoreToCache =
          Decode.field "StoreToCache" (Decode.array Decode.string) |> Decode.map StoreToCache

      Decode.oneOf [ decodeStoreAll; decodeStoreNone; decodeStoreToCache ]

  type CacheRetrieval =
    | CheckForCached of string array
    | CheckForAll
    | SkipCache // full reload
    member x.ToHeader() =
      match x with
      | CheckForCached arr ->
        let arrStr = arr |> Array.reduce (fun x y -> $"{x},{y}")
        $"CheckForCached {arrStr}"
      | SkipCache -> "SkipCache"
      | CheckForAll -> "CheckForAll"
    static member decoder =
      let decodeSkipCache =
          Decode.string
            |> Decode.andThen
            (function
              | "SkipCache" -> Decode.succeed SkipCache
              | a -> Decode.fail $"Cannot decode Cache Retrieval from given input: {a}")
      let decodeCheckForAll =
          Decode.string
            |> Decode.andThen
            (function
              | "CheckForAll" -> Decode.succeed CheckForAll
              | a -> Decode.fail $"Cannot decode Cache Retrieval from given input: {a}")     
      
      let decodeCheckForCached =
          Decode.field "CheckForCached" (Decode.array Decode.string) |> Decode.map CheckForCached

      Decode.oneOf [ decodeSkipCache; decodeCheckForAll; decodeCheckForCached ]

  type ProgressBar =
    | ShowProgressBar
    | HideProgressBar

  type PropsToEval =
    | Lazy
    | Eager
    | EagerOnly of string array

    static member decoder =
      let decodeEager =
          Decode.string
            |> Decode.andThen
            (function
              | "Eager" -> Decode.succeed Eager
              | a -> Decode.fail $"Cannot decode PropsToEval from given input: {a}")

      let decodeLazy =
          Decode.string
            |> Decode.andThen
            (function
              | "Lazy" -> Decode.succeed Lazy
              | a -> Decode.fail $"Cannot decode PropsToEval from given input: {a}")
      
      let decodeEagerOnly =
          Decode.field "EagerOnly" (Decode.array Decode.string) |> Decode.map EagerOnly

      Decode.oneOf [ decodeEager; decodeLazy; decodeEagerOnly ]
      
  type RealTimePredicates =
      | ComponentIsOneOf of string array
      | ComponentIsAny
      | ComponentIsAnyExcept of string array
      | UserIdIsOneOf of string array
      static member decoder =
        let decoder : Decoder<RealTimePredicates> =

          let decodeUserIdIsOneOf =
              Decode.field "UserIdIsOneOf" (Decode.array Decode.string) |> Decode.map UserIdIsOneOf
              
          let decodeComponentIsAny =
              Decode.string
                |> Decode.andThen
                (function
                | "ComponentIsAny" -> Decode.succeed ComponentIsAny
                | a -> Decode.fail $"Cannot decode RealtimePredicate: {a}")       
          
          let decodeComponentIsOneOf =
              Decode.field "ComponentIsOneOf" (Decode.array Decode.string) |> Decode.map ComponentIsOneOf

          let decodeComponentIsAnyExcept =
              Decode.field "ComponentIsAnyExcept" (Decode.array Decode.string) |> Decode.map ComponentIsAnyExcept

          // Now that we know how to handle each case, we say that
          // at least of the decoder should succeed to be a valid `Query` representation
          Decode.oneOf [ decodeUserIdIsOneOf ; decodeComponentIsOneOf ; decodeComponentIsAnyExcept ; decodeComponentIsAny ]
        decoder
        
  type Predicates =
    {
      predicates : RealTimePredicates array
      propsToEval : PropsToEval
    }
    
  type InertialSSEEvent =
    {
      id : Guid
      title : string
      connectionId : string option
      predicates : Predicates
      origin : string
      firedOn : DateTime
      cacheStorage: CacheStorage
      cacheRetrieval: CacheRetrieval
    }
    static member decoder =
      Decode.object (fun get ->
        {
          id = get.Required.Field "id" Decode.guid
          title = get.Required.Field "title" Decode.string
          connectionId =  get.Optional.Field "connectionId" Decode.string
          predicates = get.Required.Field "predicates" (Decode.object (fun get -> { predicates = get.Required.Field "predicates" (Decode.array RealTimePredicates.decoder) ; propsToEval = get.Required.Field "propsToEval" PropsToEval.decoder }) )// (Decode.tuple2 (Decode.array RealTimePredicates.decoder) PropsToEval.decoder)
          origin = get.Required.Field "origin" Decode.string
          firedOn = get.Required.Field "firedOn" Decode.datetimeUtc
          cacheStorage = get.Required.Field "cacheStorage" CacheStorage.decoder
          cacheRetrieval = get.Required.Field "cacheRetrieval" CacheRetrieval.decoder 
        })
      
  type ReloadOnMount =
    {
      shouldReload : bool
      propsToEval : PropsToEval option
      cacheStorage : CacheStorage
      cacheRetrieval : CacheRetrieval
    }
    
  type PageObj<'Props,'Shared> =
    {
        ``component`` : string
        connectionId : string option
        version : string
        url : string
        title : string
        props : 'Props option
        refreshOnBack : bool
        reloadOnMount : ReloadOnMount
        realTime : bool
        shared : 'Shared option
        urlComponentMap : (string * string) array
    }
    static member emptyObj url : PageObj<'Props,'Shared> =
      {
        ``component`` = ""
        connectionId = None
        version = ""
        url = url
        title = ""
        props = None
        refreshOnBack = false
        reloadOnMount = { shouldReload = false; propsToEval = None ; cacheStorage = CacheStorage.StoreNone ; cacheRetrieval = CacheRetrieval.SkipCache }
        realTime = true
        shared = None
        urlComponentMap = [||]
    }

    static member fromJson (json:string) propsDecoder sharedDecoder =
      
      let decodeProps (componentName:string) =
        Decode.object (fun get ->
            //get.Required.Field componentName (propsDecoder componentName)
            get.Required.Field componentName (propsDecoder componentName)
          )

      let decoder : Decoder<PageObj<'Props,'Shared>> =
        Decode.object (fun get ->
          let componentName = get.Required.Field "component" Decode.string
          

          //let asyncData = get.Required.Field "asyncData" (Decode.array (Decode.tuple2 Decode.string Decode.string))

          {
            ``component`` = componentName
            version = get.Required.Field "version" Decode.string
            connectionId =  get.Optional.Field "connectionId" Decode.string
            url = get.Required.Field "url" Decode.string
            title = get.Required.Field "title" Decode.string
            refreshOnBack = get.Required.Field "refreshOnBack" Decode.bool
            reloadOnMount = 
              get.Required.Field 
                "reloadOnMount" 
                (Decode.object 
                  (fun get -> 
                    { 
                      shouldReload = get.Required.Field "shouldReload" Decode.bool 
                      propsToEval = get.Optional.Field "propsToEval" PropsToEval.decoder
                      cacheStorage = get.Required.Field "cacheStorage" CacheStorage.decoder
                      cacheRetrieval = get.Required.Field "cacheRetrieval" CacheRetrieval.decoder
                    }) ) // Decode.bool PropsToEval.decoder)
            realTime = get.Required.Field "realTime" Decode.bool
            props = get.Required.Field "props" (decodeProps componentName)
            shared = get.Required.Field "shared" sharedDecoder
            urlComponentMap = get.Required.Field "urlComponentMap" (Decode.array (Decode.tuple2 Decode.string Decode.string))
          }
        )
      Decode.fromString decoder json

  // type IPage =
  //   static abstract fields: string array
  //   abstract toMap: string array option -> (string * obj) array
  //   abstract resolve: Map<string,obj> -> IPage
  //   static abstract decoder: (string -> obj -> Result<IPage,DecoderError>)