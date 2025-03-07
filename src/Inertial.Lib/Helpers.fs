namespace Inertial.Lib

open Thoth.Json

module Helpers =
    
    /// Assumes cached data is stored in a Map format of ["Choice2Of2", ["Ok", { field.. : value.. } ] ]
    let cacheResultDecoder<'T> =
        Decode.index 0 Decode.string
        |> Decode.andThen (fun result ->
            match result with
            | "Choice2Of2" -> 
              Decode.index 1 (
                  Decode.index 0 Decode.string |> Decode.andThen (fun inner ->
                    match inner with
                    | "Ok" ->
                      Decode.index 1 Decode.value |>
                        Decode.andThen (fun result ->
                            Decode.succeed <| (Choice2Of2 (Ok result))
                          )
                      
                    | invalid -> Decode.fail $"""Error decoding cached value, got: "%s{invalid}"""
                    )
                
                )

            | invalid ->
              Decode.fail $"""Error decoding cached value, got: "%s{invalid}""")
    
    /// Assumes cached data is stored in a Map format of ["Choice2Of2", { field.. : value.. } ]
    let cacheObjectDecoder<'T> =
      Decode.index 0 Decode.string
      |> Decode.andThen (fun result ->
          match result with
          | "Choice2Of2" -> 
            Decode.index 1 Decode.value |>
              Decode.andThen (fun result ->
                //printfn $"result: {result}"
                  Decode.succeed <| (Choice2Of2 result)
                )

          | invalid ->
            Decode.fail $"""Error decoding cached value, got: "%s{invalid}""")
    
    /// Assumes cached data is stored in a Map format of ["Choice2Of2", [ { field.. : value.. }, { field2.. : value2.. } ] ]  
    let cacheListDecoder<'T> =
      Decode.index 0 Decode.string
      |> Decode.andThen (fun result ->
          match result with
          | "Choice2Of2" -> 
            // Decode.index 1 Decode.value |>
            //   Decode.andThen (fun result ->
            //     //printfn $"result: {result}"
            //       Decode.succeed <| ListOfObjects (Choice2Of2 <| Decode.list result)
            //     )
            Decode.index 1 (Decode.list Decode.value) |>
              Decode.andThen (fun list ->
                  Decode.succeed <| (Choice2Of2 list)
                )

          | invalid ->
            Decode.fail $"""Error decoding cached value, got: "%s{invalid}""")

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
        
    let asyncChoice2ListDecoder<'T> (decoder: Decoder<'T>) =
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
                  Decode.field "Choice2Of2" (Decode.list (resultDecoder decoder)) |> Decode.map Choice2Of2
            

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
