namespace Inertial.Lib

open Thoth.Json

module Helpers =
       
    
    let mapDecoderToOpt decoder = decoder |> Decode.map Some
    
    let asyncDataDecoder<'T> =
        Decode.index 0 Decode.string
        |> Decode.andThen (fun result ->
            match result with
            | "Choice2List" ->
                Decode.index 1 (Decode.index 0 Decode.string
                    |> Decode.andThen (fun result ->
                        match result with
                        // | "Choice1Of2" ->
                        //     Decode.index 1 (
                        //         Decode.index 0 Decode.value |> Decode.andThen ( fun list -> list ))
                        | "Choice2Of2" ->
                            Decode.index 1 (Decode.list Decode.value) |>
                              Decode.andThen (fun list ->
                                  Choice2Of2 list |> Decode.succeed |> Decode.map AsyncData.Choice2List
                                )
                        | invalid -> Decode.fail $"""Error decoding cached value, got: "%s{invalid}"""))
            | "Choice2ResultList" ->
                Decode.index 1 (Decode.index 0 Decode.string
                |> Decode.andThen (fun result ->
                    match result with
                    | "Choice1Of2" ->
                        Decode.fail $"""Async data failed to load properly"""
                    | "Choice2Of2" -> 
                          Decode.index 1 (
                              Decode.index 0 Decode.string |> Decode.andThen (fun inner ->
                                match inner with
                                | "Ok" ->
                                  Decode.index 1 (Decode.list Decode.value) |>
                                      Decode.andThen (fun list ->
                                          Choice2Of2 (Ok list) |> Decode.succeed |> Decode.map AsyncData.Choice2ResultList
                                        )
                                | "Error" ->
                                  Decode.index 1 Decode.string |>
                                      Decode.andThen (fun err ->
                                          Choice2Of2 (Error err) |> Decode.succeed |> Decode.map AsyncData.Choice2ResultList
                                        )
                                  
                                | invalid -> Decode.fail $"""Error decoding cached Choice2 value, got: "%s{invalid}"""
                                )
                            
                            )
                    | invalid -> Decode.fail $"""Error decoding cached value, got: "%s{invalid}"""
                ))
            | invalid -> Decode.fail $"""Error decoding cached value, got: "%s{invalid}"""
            
        )
    
    /// Assumes cached data is stored in a Map format of ["Choice2Of2", ["Ok", { field.. : value.. } ] ]
    // let cacheResultDecoder<'T> =
    //     Decode.index 0 Decode.string
    //     |> Decode.andThen (fun result ->
    //         match result with
    //         | "Choice2Of2" -> 
    //           Decode.index 1 (
    //               Decode.index 0 Decode.string |> Decode.andThen (fun inner ->
    //                 match inner with
    //                 | "Ok" ->
    //                   Decode.index 1 Decode.value |>
    //                     Decode.andThen (fun result ->
    //                         Decode.succeed <| (Choice2Of2 (Ok result))
    //                       )
    //                   
    //                 | invalid -> Decode.fail $"""Error decoding cached value, got: "%s{invalid}"""
    //                 )
    //             
    //             )
    //
    //         | invalid ->
    //           Decode.fail $"""Error decoding cached value, got: "%s{invalid}""")
    //
    // /// Assumes cached data is stored in a Map format of ["Choice2Of2", { field.. : value.. } ]
    // let cacheObjectDecoder<'T> =
    //   Decode.index 0 Decode.string
    //   |> Decode.andThen (fun result ->
    //       match result with
    //       | "Choice2Of2" -> 
    //         Decode.index 1 Decode.value |>
    //           Decode.andThen (fun result ->
    //             //printfn $"result: {result}"
    //               Decode.succeed <| (Choice2Of2 result)
    //             )
    //
    //       | invalid ->
    //         Decode.fail $"""Error decoding cached value, got: "%s{invalid}""")
    //
    // /// Assumes cached data is stored in a Map format of ["Choice2Of2", [ { field.. : value.. }, { field2.. : value2.. } ] ]  
    // let cacheListDecoder<'T> =
    //   Decode.index 0 Decode.string
    //   |> Decode.andThen (fun result ->
    //       match result with
    //       | "Choice2Of2" -> 
    //         // Decode.index 1 Decode.value |>
    //         //   Decode.andThen (fun result ->
    //         //     //printfn $"result: {result}"
    //         //       Decode.succeed <| ListOfObjects (Choice2Of2 <| Decode.list result)
    //         //     )
    //         Decode.index 1 (Decode.list Decode.value) |>
    //           Decode.andThen (fun list ->
    //               Decode.succeed <| (Choice2Of2 list)
    //             )
    //
    //       | invalid ->
    //         Decode.fail $"""Error decoding cached value, got: "%s{invalid}""")

    
    let emptyDecoder : Decoder<obj> =
        Decode.object( fun _ -> null )
        
        
    let resultDecoder<'T> (decoder: Decoder<'T>) =
        
        let decoder: Decoder<Result<'T,string>> =                
                let decodeOK =
                      Decode.field "Ok" decoder |> Decode.map Ok
                let decodeError =
                      Decode.field "Error" (Decode.string) |> Decode.map Error

                Decode.oneOf [ decodeOK ; decodeError ]
                
        decoder
        
    let asyncChoice2OptionDecoder<'T> (decoder: Decoder<'T>) =
        let decoder =
            let returnOption : Option<'T> = None
            
            let decodeChoice1 =
                  Decode.field "Choice1Of2" (emptyDecoder |> Decode.andThen (fun _ ->  Decode.succeed (Choice1Of2 <| async { return returnOption }  ) ))
            
            let decodeChoice2 =
                  Decode.field "Choice2Of2" (Decode.option decoder) |> Decode.map Choice2Of2
            

            Decode.oneOf [ decodeChoice1 ; decodeChoice2 ]
            
        decoder
        
    let asyncChoice2OptionListDecoder<'T> (decoder: Decoder<'T>) =
        let decoder =
            let returnOption : Option<'T list> = None
            
            let decodeChoice1 =
                  Decode.field "Choice1Of2" (emptyDecoder |> Decode.andThen (fun _ ->  Decode.succeed (Choice1Of2 <| async { return returnOption }  ) ))
            
            let decodeChoice2 =
                  Decode.field "Choice2Of2" (Decode.option <| Decode.list decoder) |> Decode.map Choice2Of2
            

            Decode.oneOf [ decodeChoice1 ; decodeChoice2 ]
            
        decoder
        
    let asyncChoice2ListDecoder<'T> (decoder: Decoder<'T>) =
        let decoder =                
            let returnList : List<'T> = []
            
            let decodeChoice1 =
                  Decode.field "Choice1Of2" (emptyDecoder |> Decode.andThen (fun _ ->  Decode.succeed (Choice1Of2 <| async { return returnList }  ) ))
            
            let decodeChoice2 =
                  Decode.field "Choice2Of2" (Decode.list decoder) |> Decode.map Choice2Of2
            

            Decode.oneOf [ decodeChoice1 ; decodeChoice2 ]
            
        decoder
        
    let asyncChoice2ListDecoderFromValue<'T> (decoder: Decoder<'T>) =
        let decoder =                
            let returnList : List<'T> = []

            let decodeChoice1 =
                  Decode.field "Choice1Of2" (emptyDecoder |> Decode.andThen (fun _ ->  Decode.succeed (Choice1Of2 <| async { return returnList }  ) ))
            
            let decodeChoice2 =
                  Decode.field "Choice2Of2" (Decode.list decoder) |> Decode.map Choice2Of2
            

            Decode.oneOf [ decodeChoice1 ; decodeChoice2 ]
            
        decoder
                
    let asyncChoice2ResultListDecoder<'T> (decoder: Decoder<'T>) =
        let decoder =                
            let returnResult : Result<List<'T>,string> = Ok []
            
            let decodeChoice1 =
                  Decode.field "Choice1Of2" (emptyDecoder |> Decode.andThen (fun _ ->  Decode.succeed (Choice1Of2 <| async { return returnResult }  ) ))
            
            let decodeChoice2 =
                  Decode.field "Choice2Of2" (resultDecoder (Decode.list decoder)) |> Decode.map Choice2Of2
            

            Decode.oneOf [ decodeChoice1 ; decodeChoice2 ]
            
        decoder
    
    let choice2CacheDecoder<'T> (decoder: Decoder<'T>) =
        Decode.field "Choice2Of2" (resultDecoder decoder) |> Decode.map Choice2Of2
        
    let optionChoice2Decoder<'T> (decoder: Decoder<'T>) =
           
            let decoder: Decoder<Choice<Option<'T>,string>> =                
                let decodeChoice1 =
                      Decode.field "Choice1Of2" (Decode.option decoder) |> Decode.map Choice1Of2
                let decodeChoice2 =
                      Decode.field "Choice2Of2" (Decode.string) |> Decode.map Choice2Of2

                Decode.oneOf [ decodeChoice1 ; decodeChoice2 ]
                
            decoder

    let decoderForAsyncData (innerDecoder: Decoder<'T>) =
        let decodeChoice2List =
            Decode.field "Choice2List" (asyncChoice2ListDecoder innerDecoder) |> Decode.map Choice2List
        let decodeChoice2Option =
            Decode.field "Choice2Option" (asyncChoice2OptionDecoder innerDecoder) |> Decode.map Choice2Option
        let decodeChoice2OptionList =
            Decode.field "Choice2OptionList" (asyncChoice2OptionListDecoder innerDecoder) |> Decode.map Choice2OptionList
        let decodeChoice2ResultList =
            Decode.field "Choice2ResultList" (asyncChoice2ResultListDecoder innerDecoder) |> Decode.map Choice2ResultList
            
        
        Decode.oneOf [
            decodeChoice2List ; decodeChoice2Option ; decodeChoice2OptionList; decodeChoice2ResultList
        ]

    let decodeCacheFromString stored = Decode.fromString asyncDataDecoder stored
    
    let decodeSSEFromString json = Decode.fromString InertialSSEEvent.decoder json
    
    let encodeBodyContent (dataMap: Map<string,obj>) = Encode.Auto.toString(4,dataMap)
    
    let encodeCacheObj (cacheObj: obj) = Encode.Auto.toString cacheObj
        