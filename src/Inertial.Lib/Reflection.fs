namespace Inertial.Lib

open FSharp.Reflection

module Reflection =
    
    let inline toMap (props: 'T) (toInclude:option<string array>) =
        let fields = FSharpType.GetRecordFields(typeof<'T>) |> Array.map (_.Name)
        let values = FSharpValue.GetRecordFields(props)
        let map = 
            Array.zip fields values
        //printfn $"{values}"
        let inner =
            match toInclude with
            | Some toInclude -> 
                map |> Array.filter (fun (k,v) -> toInclude |> Array.contains k)
            | None -> map
        inner
      
    let inline fields<'T> =
        FSharpType.GetRecordFields(typeof<'T>)
        |> Array.map (fun z ->
            let string = z.PropertyType.ToString()
            let parts = string.Split('[')
            if parts |> Array.isEmpty then None
            else
               if parts[0] = "Microsoft.FSharp.Core.FSharpChoice`2" then Some z else None
            )
        |> Array.choose id
        |> Array.map (_.Name)
        
    let inline resolve (oldProps:'T) (newPropsMap: Map<string,obj>) =
        let oldPropsMap = toMap oldProps None

        let merged = 
             oldPropsMap 
             |> Array.map (fun (k,v) -> 
                 if newPropsMap.ContainsKey k then
                     newPropsMap[k]
                 else 
                     v)
             
        FSharpValue.MakeRecord(typeof<'T>, merged) :?> 'T
